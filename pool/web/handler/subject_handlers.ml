module Command = Cqrs_command.Subject_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module User = Pool_user

let create_layout req = General.create_tenant_layout `Subject req

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    Page.Subject.dashboard context
    |> create_layout req ~active_navigation:"/dashboard" context
    >|= Sihl.Web.Response.of_html
    |> Lwt_result.map_err (fun err -> err, "/index")
  in
  result |> HttpUtils.extract_happy_path req
;;

let sign_up req =
  let result context =
    let open Lwt_result.Syntax in
    let open Lwt_result.Infix in
    let open Pool_common.Message in
    Lwt_result.map_err (fun err -> err, "/index")
    @@
    let go field = field |> Field.show |> CCFun.flip Sihl.Web.Flash.find req in
    let channels = Subject.RecruitmentChannel.(all |> CCList.map show) in
    let email = go Field.Email in
    let firstname = go Field.Firstname in
    let lastname = go Field.Lastname in
    let recruitment_channel = go Field.RecruitmentChannel in
    let tenant_db = context.Pool_context.tenant_db in
    let language = context.Pool_context.language in
    let* terms = Settings.terms_and_conditions tenant_db language in
    Page.Subject.sign_up
      channels
      email
      firstname
      lastname
      recruitment_channel
      terms
      context
    |> create_layout req ~active_navigation:"/signup" context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let sign_up_create req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let terms_key = Field.(TermsAccepted |> show) in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun msg ->
        msg, "/signup", [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let* () =
         CCList.assoc ~eq:( = ) terms_key urlencoded
         |> CCList.hd
         |> CCString.equal "true"
         |> Utils.Bool.to_result TermsAndConditionsNotAccepted
         |> Lwt_result.lift
       in
       let tenant_db = context.Pool_context.tenant_db in
       let* remove_subject_event =
         let find_subject email =
           email
           |> Subject.find_by_email tenant_db
           ||> function
           | Ok partitipant ->
             if partitipant.Subject.user.Sihl_user.confirmed
             then Error EmailAlreadyInUse
             else Ok (Some partitipant)
           | Error _ -> Ok None
         in
         Sihl.Web.Request.urlencoded Field.(Email |> show) req
         ||> CCOption.to_result SubjectSignupInvalidEmail
         >== Pool_user.EmailAddress.create
         >>= find_subject
         >>= CCOption.map_or ~default:(Lwt_result.return []) (fun p ->
                 Command.DeleteUnverified.handle p |> Lwt_result.lift)
       in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes tenant_db
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let preferred_language = HttpUtils.browser_language_from_req req in
       let* events =
         let open CCResult.Infix in
         Command.SignUp.(
           decode urlencoded
           >>= handle ?allowed_email_suffixes preferred_language)
         >>= (fun e -> Ok (remove_subject_event @ e))
         |> Lwt_result.lift
       in
       let query_lang = context.Pool_context.query_language in
       Utils.Database.with_transaction tenant_db (fun () ->
           let%lwt () = Pool_event.handle_events tenant_db events in
           HttpUtils.(
             redirect_to_with_actions
               (path_with_language query_lang "/email-confirmation")
               [ Message.set
                   ~success:[ Pool_common.Message.EmailConfirmationMessage ]
               ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  let result context =
    let open Lwt_result.Syntax in
    let open Pool_common.Message in
    let tenant_db = context.Pool_context.tenant_db in
    let%lwt redirect_path =
      let%lwt user = Http_utils.user_from_session tenant_db req in
      CCOption.bind user (fun user ->
          Some (General.dashboard_path tenant_db user))
      |> CCOption.value ~default:("/login" |> Lwt.return)
    in
    (let* token =
       Sihl.Web.Request.query Field.(show Token) req
       |> CCOption.map Email.Token.create
       |> CCOption.to_result Field.(NotFound Token)
       |> Lwt_result.lift
     in
     let ctx = Pool_tenant.to_ctx tenant_db in
     let* email =
       Service.Token.read
         ~ctx
         (Email.Token.value token)
         ~k:Field.(Email |> show)
       ||> CCOption.to_result TokenInvalidFormat
       >== Pool_user.EmailAddress.create
       >>= Email.find_unverified_by_address tenant_db
       |> Lwt_result.map_err (fun _ -> Field.(Invalid Token))
     in
     let* subject = Subject.find tenant_db (Email.user_id email) in
     let* events =
       match subject.Subject.user.Sihl.Contract.User.confirmed with
       | false ->
         Command.VerifyEmail.(handle { email } subject) |> Lwt_result.lift
       | true -> Command.UpdateEmail.(handle subject email) |> Lwt_result.lift
     in
     let%lwt () = Pool_event.handle_events tenant_db events in
     HttpUtils.(
       redirect_to_with_actions
         (path_with_language context.Pool_context.query_language redirect_path)
         [ Message.set ~success:[ EmailVerified ] ])
     |> Lwt_result.ok)
    |> Lwt_result.map_err (fun msg -> msg, redirect_path)
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result context =
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun err -> err, "/login")
    @@ let* user =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
       in
       let language = context.Pool_context.language in
       let* terms = Settings.terms_and_conditions tenant_db language in
       Page.Subject.terms user.Sihl_user.id terms context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms_accept req =
  let result context =
    let query_lang = context.Pool_context.query_language in
    Lwt_result.map_err (fun msg -> msg, "/login")
    @@
    let open Lwt_result.Syntax in
    let id =
      Pool_common.(
        Sihl.Web.Router.param req Message.Field.(Id |> show) |> Id.of_string)
    in
    let tenant_db = context.Pool_context.tenant_db in
    let* subject = Subject.find tenant_db id in
    let* events =
      Command.AcceptTermsAndConditions.handle subject |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    HttpUtils.(redirect_to (path_with_language query_lang "/dashboard"))
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let user_update_csrf = "_user_update_csrf"

let show is_edit req =
  let result context =
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun err -> err, "/login")
    @@ let* user =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
       in
       let* subject =
         Subject.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
         |> Lwt_result.map_err (fun err -> err)
       in
       match is_edit with
       | false ->
         Page.Subject.detail subject context
         |> create_layout req context
         >|= Sihl.Web.Response.of_html
       | true ->
         let* tenant_languages =
           Pool_context.Tenant.find req
           |> Lwt_result.lift
           >|= fun c -> c.Pool_context.Tenant.tenant_languages
         in
         Page.Subject.edit user_update_csrf subject tenant_languages context
         |> create_layout req ~active_navigation:"/user/edit" context
         >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let details = show false
let edit = show true

let update req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values Field.[ Paused |> show ]
  in
  let result context =
    let query_lang = context.Pool_context.query_language in
    let path_with_lang = HttpUtils.path_with_language query_lang in
    let go name = CCList.assoc ~eq:String.equal name urlencoded |> CCList.hd in
    let version_raw = go "version" in
    let name = go "field" |> Field.read in
    let open Utils.Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* user =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result (NotFound Field.User, path_with_lang "/login")
    in
    let* subject =
      Subject.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_err (fun err -> err, path_with_lang "/login")
    in
    let language = context.Pool_context.language in
    let* tenant_context =
      Pool_context.Tenant.find req
      |> Lwt_result.lift
      |> Lwt_result.map_err (fun err -> err, path_with_lang "/user/edit")
    in
    let version =
      version_raw
      |> CCInt.of_string
      |> CCOption.get_exn_or
           (Pool_common.Utils.error_to_string
              language
              (NotANumber Field.(name |> show)))
    in
    let get_version =
      CCOption.get_exn_or
        (Pool_common.Utils.error_to_string
           language
           (NotHandled Field.(name |> show)))
    in
    let current_version =
      Subject.version_selector subject Field.(name |> show) |> get_version
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Subject_command.Update in
      if Pool_common.Version.value current_version <= version
      then urlencoded |> decode >>= handle subject
      else Error (MeantimeUpdate name)
    in
    let hx_post = Sihl.Web.externalize_path (path_with_lang "/user/update") in
    let csrf = context.Pool_context.csrf in
    let htmx_element subject classnames ?error () =
      let open Subject in
      let csrf_element = Htmx.csrf_element_swap csrf ~id:user_update_csrf () in
      let html_response input =
        [ Htmx.create input language ~classnames ~hx_post ?error ()
        ; csrf_element
        ]
        |> HttpUtils.multi_html_to_plain_text_response
      in
      Lwt_result.return
      @@
      match[@warning "-4"] name with
      | Field.Paused ->
        Htmx.Paused (subject.paused_version, subject.paused) |> html_response
      | Field.Firstname ->
        Htmx.Firstname (subject.firstname_version, subject |> firstname)
        |> html_response
      | Field.Lastname ->
        Htmx.Lastname (subject.lastname_version, subject |> lastname)
        |> html_response
      | Field.Language ->
        (match error with
        | Some _ ->
          Htmx.Language
            ( subject.language_version
            , subject.language
            , tenant_context.Pool_context.Tenant.tenant_languages )
          |> html_response
        | None ->
          Sihl.Web.Response.of_plain_text ""
          |> Sihl.Web.Response.add_header ("HX-Redirect", "/user/edit"))
      | k ->
        failwith
        @@ Pool_common.Utils.error_to_string
             language
             (NotHandled (k |> Field.show))
    in
    match events with
    | Ok events ->
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      let* subject =
        Subject.(subject |> id |> find tenant_db)
        |> Lwt_result.map_err (fun err -> err, "/login")
      in
      htmx_element subject [ "success" ] ()
    | Error err -> htmx_element subject [ "error" ] ~error:err ()
  in
  Lwt.catch
    (fun () -> result |> HttpUtils.extract_happy_path req)
    (fun exn ->
      Logs.err (fun m -> m "%s" @@ Printexc.to_string exn);
      Sihl.Web.Response.of_plain_text ""
      |> Sihl.Web.Response.add_header ("HX-Redirect", "/error")
      |> Lwt.return)
;;

let update_email req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    Lwt_result.map_err (fun msg ->
        HttpUtils.(msg, "/user/edit", [ urlencoded_to_flash urlencoded ]))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* subject =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result (NotFound Field.User)
      >>= fun user ->
      Subject.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
    in
    let%lwt allowed_email_suffixes =
      let open Utils.Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db
      ||> fun suffixes ->
      if CCList.is_empty suffixes then None else Some suffixes
    in
    let* new_email =
      Pool_user.EmailAddress.create
        (CCList.assoc ~eq:CCString.equal Field.(Email |> show) urlencoded
        |> CCList.hd)
      |> Lwt_result.lift
    in
    let* events =
      Command.RequestEmailValidation.(
        handle ?allowed_email_suffixes subject new_email |> Lwt_result.lift)
    in
    Utils.Database.with_transaction tenant_db (fun () ->
        let%lwt () = Pool_event.handle_events tenant_db events in
        HttpUtils.(
          redirect_to_with_actions
            (path_with_language query_lang "/email-confirmation")
            [ Message.set ~success:[ EmailConfirmationMessage ] ]))
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update_password req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun msg ->
        HttpUtils.(msg, "/user/edit", [ urlencoded_to_flash urlencoded ]))
    @@ let* subject =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
         >>= fun user ->
         Subject.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       let* events =
         let open CCResult.Infix in
         Command.UpdatePassword.(decode urlencoded >>= handle subject)
         |> Lwt_result.lift
       in
       Utils.Database.with_transaction tenant_db (fun () ->
           let%lwt () = Pool_event.handle_events tenant_db events in
           HttpUtils.(
             redirect_to_with_actions
               (path_with_language query_lang "/user/edit")
               [ Message.set ~success:[ Pool_common.Message.PasswordChanged ] ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;
