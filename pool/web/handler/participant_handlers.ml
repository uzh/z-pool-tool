module Command = Cqrs_command.Participant_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module User = Pool_user

let dashboard req =
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  Page.Participant.dashboard message ()
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let sign_up req =
  let error_path = "/" in
  let%lwt result =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let csrf = HttpUtils.find_csrf req in
    let message =
      Sihl.Web.Flash.find_alert req
      |> CCFun.flip CCOption.bind Message.of_string
    in
    let go = CCFun.flip Sihl.Web.Flash.find req in
    let channels = Participant.RecruitmentChannel.all () in
    let email = go "email" in
    let firstname = go "firstname" in
    let lastname = go "lastname" in
    let recruitment_channel = go "recruitment_channel" in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* terms = Settings.default_language_terms_and_conditions tenant_db in
    Page.Participant.sign_up
      csrf
      message
      channels
      email
      firstname
      lastname
      recruitment_channel
      terms
      ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let sign_up_create req =
  let open Utils.Lwt_result.Infix in
  let terms_key = "_terms_accepted" in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* () =
      CCList.assoc ~eq:( = ) terms_key urlencoded
      |> CCList.hd
      |> CCString.equal "true"
      |> Utils.Bool.to_result Pool_common.Message.TermsAndConditionsNotAccepted
      |> Lwt_result.lift
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* remove_participant_event =
      let find_participant email =
        email
        |> Participant.find_by_email tenant_db
        ||> function
        | Ok partitipant ->
          if partitipant.Participant.user.Sihl_user.confirmed
          then Error Pool_common.Message.EmailAlreadyInUse
          else Ok (Some partitipant)
        | Error _ -> Ok None
      in
      Sihl.Web.Request.urlencoded "email" req
      ||> CCOption.to_result Pool_common.Message.ParticipantSignupInvalidEmail
      >== Pool_user.EmailAddress.create
      >>= find_participant
      >>= CCOption.map_or ~default:(Lwt_result.return []) (fun p ->
              Command.DeleteUnverified.handle p |> Lwt_result.lift)
    in
    let%lwt allowed_email_suffixes =
      let open Utils.Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db
      ||> fun suffixes ->
      if CCList.is_empty suffixes then None else Some suffixes
    in
    let default_language = HttpUtils.browser_language_from_req req in
    let* events =
      let open CCResult.Infix in
      Command.SignUp.(
        decode urlencoded >>= handle ?allowed_email_suffixes default_language)
      >>= (fun e -> Ok (remove_participant_event @ e))
      |> Lwt_result.lift
    in
    Utils.Database.with_transaction tenant_db (fun () ->
        let%lwt () = Pool_event.handle_events tenant_db events in
        HttpUtils.redirect_to_with_actions
          "/email-confirmation"
          [ Message.set
              ~success:[ Pool_common.Message.EmailConfirmationMessage ]
          ])
    |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun msg ->
         msg, "/signup", [ HttpUtils.urlencoded_to_flash urlencoded ])
  |> HttpUtils.extract_happy_path_with_actions
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  let result =
    let open Lwt_result.Syntax in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, "/")
    in
    let%lwt redirect_path =
      let%lwt user = General.user_from_session tenant_db req in
      CCOption.bind user (fun user ->
          Some (General.dashboard_path tenant_db user))
      |> Option.value ~default:("/login" |> Lwt.return)
    in
    (let* token =
       Sihl.Web.Request.query "token" req
       |> CCOption.map Email.Token.create
       |> CCOption.to_result Pool_common.Message.(NotFound Token)
       |> Lwt_result.lift
     in
     let ctx = Pool_tenant.to_ctx tenant_db in
     let* email =
       Service.Token.read ~ctx (Email.Token.value token) ~k:"email"
       ||> CCOption.to_result Pool_common.Message.TokenInvalidFormat
       >== Pool_user.EmailAddress.create
       >>= Email.find_unverified_by_address tenant_db
       |> Lwt_result.map_err (fun _ -> Pool_common.Message.(Invalid Token))
     in
     let* participant = Participant.find tenant_db (Email.user_id email) in
     let* events =
       match participant.Participant.user.Sihl.Contract.User.confirmed with
       | false ->
         Command.VerifyAccount.(handle { email } participant) |> Lwt_result.lift
       | true ->
         Command.UpdateEmail.(handle participant email) |> Lwt_result.lift
     in
     let%lwt () = Pool_event.handle_events tenant_db events in
     HttpUtils.redirect_to_with_actions
       redirect_path
       [ Message.set ~success:[ Pool_common.Message.EmailVerified ] ]
     |> Lwt_result.ok)
    |> Lwt_result.map_err (fun msg -> msg, redirect_path)
  in
  result >|> HttpUtils.extract_happy_path
;;

let terms req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let csrf = HttpUtils.find_csrf req in
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  let%lwt result =
    let error_path = "/login" in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, "/")
    in
    let* user =
      General.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User, error_path)
    in
    let* participant =
      Participant.find
        tenant_db
        (user.Sihl.Contract.User.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_err (fun err -> err, error_path)
    in
    let* terms =
      Settings.user_language_terms_and_conditions
        tenant_db
        (Participant.preferred_language participant)
      |> Lwt_result.map_err (fun err -> err, error_path)
    in
    Page.Participant.terms csrf message user.Sihl_user.id terms ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let terms_accept req =
  let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
  let%lwt result =
    Lwt_result.map_err (fun msg -> msg, "/login")
    @@
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* participant = Participant.find tenant_db id in
    let* events =
      Command.AcceptTermsAndConditions.handle participant |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    HttpUtils.redirect_to "/dashboard" |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path
;;

let user_update_csrf = "_user_update_csrf"

let show is_edit req =
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, "/")
    in
    let* user =
      General.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User, "/login")
    in
    let* participant =
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_err (fun err -> err, "/login")
    in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    match is_edit with
    | false ->
      Page.Participant.detail participant message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
    | true ->
      let csrf = HttpUtils.find_csrf req in
      Page.Participant.edit csrf user_update_csrf participant message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let details = show false
let edit = show true

let update req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values [ "paused" ]
  in
  let result () =
    let go name = CCList.assoc ~eq:String.equal name urlencoded |> CCList.hd in
    let version_raw = go "version" in
    let name = go "field" in
    let version =
      version_raw
      |> CCInt.of_string
      |> CCOption.get_exn_or
         @@ Format.asprintf "Version '%s' not a number" version_raw
    in
    let open Utils.Lwt_result.Syntax in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, "/")
    in
    let* user =
      General.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User, "/login")
    in
    let* participant =
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_err (fun err -> err, "/login")
    in
    let value = go name in
    let get_version =
      CCOption.get_exn_or
        (Format.asprintf "No version found for field '%s'" name)
    in
    let current_version =
      Participant.version_selector participant name |> get_version
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Participant_command.UpdateDetails in
      if Pool_common.Version.value current_version <= version
      then urlencoded |> decode >>= handle participant
      else
        let open Pool_common.Message in
        let err_field =
          match name with
          | "firstname" -> Firstname
          | "lastname" -> Lastname
          | "paused" -> Paused
          | k -> failwith @@ Format.asprintf "Field '%s' is not handled" k
        in
        Error (MeantimeUpdate err_field)
    in
    let hx_post = Sihl.Web.externalize_path "/user/update" in
    let csrf = HttpUtils.find_csrf req in
    let base_input version =
      let type_of = function
        | "paused" -> `Checkbox
        | "firstname" | "lastname" -> `Text
        | k -> failwith @@ Format.asprintf "Field '%s' is not handled" k
      in
      Component.hx_input_element
        (type_of name)
        name
        value
        version
        ~hx_post
        ~hx_params:[ name ]
    in
    match events with
    | Ok events ->
      let open Utils.Lwt_result.Syntax in
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      let* participant =
        Participant.(participant |> id |> find tenant_db)
        |> Lwt_result.map_err (fun err -> err, "/login")
      in
      [ base_input
          (Participant.version_selector participant name |> get_version)
          ~classnames:[ "success" ]
          ()
      ; Component.csrf_element_swap csrf ~id:user_update_csrf ()
      ]
      |> HttpUtils.multi_html_to_plain_text_response
      |> Lwt_result.return
    | Error err ->
      [ base_input current_version ~classnames:[ "error" ] ~error:err ()
      ; Component.csrf_element_swap csrf ~id:user_update_csrf ()
      ]
      |> HttpUtils.multi_html_to_plain_text_response
      |> Lwt_result.return
  in
  Lwt.catch
    (fun () -> result () >|> HttpUtils.extract_happy_path)
    (fun exn ->
      Logs.err (fun m -> m "%s" @@ Printexc.to_string exn);
      Sihl.Web.Response.of_plain_text ""
      |> Sihl.Web.Response.add_header ("HX-Redirect", "/error")
      |> Lwt.return)
;;

let update_email req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* participant =
      General.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User)
      >>= fun user ->
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
    in
    let%lwt allowed_email_suffixes =
      let open Utils.Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db
      ||> fun suffixes ->
      if CCList.is_empty suffixes then None else Some suffixes
    in
    let* new_email =
      Pool_user.EmailAddress.create
        (CCList.assoc ~eq:CCString.equal "email" urlencoded |> CCList.hd)
      |> Lwt_result.lift
    in
    let* events =
      Command.RequestEmailValidation.(
        handle ?allowed_email_suffixes participant new_email |> Lwt_result.lift)
    in
    Utils.Database.with_transaction tenant_db (fun () ->
        let%lwt () = Pool_event.handle_events tenant_db events in
        HttpUtils.redirect_to_with_actions
          "/email-confirmation"
          [ Message.set
              ~success:[ Pool_common.Message.EmailConfirmationMessage ]
          ])
    |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun msg ->
         msg, "/user/edit", [ HttpUtils.urlencoded_to_flash urlencoded ])
  |> HttpUtils.extract_happy_path_with_actions
;;

let update_password req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* participant =
      General.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User)
      >>= fun user ->
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
    in
    let* events =
      let open CCResult.Infix in
      Command.UpdatePassword.(decode urlencoded >>= handle participant)
      |> Lwt_result.lift
    in
    Utils.Database.with_transaction tenant_db (fun () ->
        let%lwt () = Pool_event.handle_events tenant_db events in
        HttpUtils.redirect_to_with_actions
          "/user/edit"
          [ Message.set ~success:[ Pool_common.Message.PasswordChanged ] ])
    |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun msg ->
         msg, "/user/edit", [ HttpUtils.urlencoded_to_flash urlencoded ])
  |> HttpUtils.extract_happy_path_with_actions
;;
