module Command = Cqrs_command.Participant_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Email = Common_user.Email

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
    let%lwt terms = Settings.find_terms_and_conditions tenant_db in
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
    let* () =
      Sihl.Web.Request.urlencoded "email" req
      ||> CCOption.to_result Pool_common.Message.ParticipantSignupInvalidEmail
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let%lwt allowed_email_suffixes =
      let open Utils.Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db
      ||> fun suffixes ->
      if CCList.is_empty suffixes then None else Some suffixes
    in
    let* events =
      let open CCResult.Infix in
      Command.SignUp.(decode urlencoded >>= handle ?allowed_email_suffixes)
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
  (let open Lwt_result.Syntax in
  let* token =
    Sihl.Web.Request.query "token" req
    |> CCOption.map Email.Token.create
    |> CCOption.to_result Pool_common.Message.(NotFound Token)
    |> Lwt_result.lift
  in
  let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
  let ctx = Pool_common.Utils.pool_to_ctx tenant_db in
  let* email =
    Service.Token.read ~ctx (Email.Token.value token) ~k:"email"
    ||> CCOption.to_result Pool_common.Message.TokenInvalidFormat
    >== Email.Address.create
    >>= Email.find_unverified tenant_db
  in
  let* participant =
    Participant.find_by_email tenant_db (Email.address email)
  in
  let* events =
    Command.ConfirmEmail.(handle { email } participant) |> Lwt_result.lift
  in
  let%lwt () = Pool_event.handle_events tenant_db events in
  HttpUtils.redirect_to_with_actions
    "/login"
    [ Message.set ~success:[ Pool_common.Message.EmailVerified ] ]
  |> Lwt_result.ok)
  |> Lwt_result.map_err (fun msg -> msg, "/login")
  >|> HttpUtils.extract_happy_path
;;

let terms req =
  CCFun.flip Lwt.bind HttpUtils.extract_happy_path
  @@
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let csrf = HttpUtils.find_csrf req in
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  let* tenant_db =
    Middleware.Tenant.tenant_db_of_request req
    |> Lwt_result.map_err (fun err -> err, "/")
  in
  let* user =
    General.user_from_session tenant_db req
    ||> CCOption.to_result Pool_common.Message.(NotFound User, "/login")
  in
  let%lwt terms = Settings.find_terms_and_conditions tenant_db in
  Page.Participant.terms csrf message user.Sihl_user.id terms ()
  |> Sihl.Web.Response.of_html
  |> Lwt.return_ok
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
      let changeset = participant |> Participant.create_changeset in
      Page.Participant.edit csrf participant message changeset ()
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
    ||> HttpUtils.format_request_boolean_values [ "paused" ]
  in
  (* TODO [aerben] remove unsafe functions *)
  let changeset_raw = List.hd @@ List.assoc "changeset" urlencoded in
  let%lwt result =
    let open Utils.Lwt_result.Syntax in
    let changeset = Pool_common.ChangeSet.of_string changeset_raw in
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
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Participant_command.UpdateDetails in
      urlencoded
      |> Pool_common.ChangeSet.check_for_update
           changeset
           (participant |> Participant.create_changeset)
      >>= decode
      >>= handle participant
    in
    let name, values =
      (* TODO don't use hd here, dangerous *)
      urlencoded |> HttpUtils.remove_meta_from_urlencoded |> CCList.hd
    in
    let hx_target =
      Format.asprintf
        "form[data-id='%s'] div[data-name='%s']"
        (participant |> Participant.id |> Pool_common.Id.value)
        name
    in
    let hx_post = Sihl.Web.externalize_path "/user/update" in
    let base_input changeset =
      let type_of = function
        | "paused" -> `Checkbox
        | _ -> `Text
      in
      Component.hx_input_element
        (type_of name)
        name
        (CCList.hd values)
        ~changeset
        ~hx_post
        ~hx_target
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
      let update_form participant =
        base_input
          (Participant.create_changeset participant)
          ~classnames:[ "success" ]
          ()
        |> HttpUtils.html_to_plain_text_response
      in
      participant |> update_form |> Lwt_result.return
    | Error err ->
      let input = base_input [] ~classnames:[ "error" ] ~error:err () in
      input |> HttpUtils.html_to_plain_text_response |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
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
    let* current_email =
      Common_user.Email.find_verified
        tenant_db
        (Participant.email_address participant)
    in
    let* new_email =
      Common_user.Email.Address.create
        (CCList.assoc ~eq:CCString.equal "email" urlencoded |> CCList.hd)
      |> Lwt_result.lift
    in
    let* events =
      Command.UpdateEmail.(
        handle ?allowed_email_suffixes participant { current_email; new_email })
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
