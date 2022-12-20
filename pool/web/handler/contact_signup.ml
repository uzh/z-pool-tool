module Command = Cqrs_command.Contact_command
module UserCommand = Cqrs_command.User_command
module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let sign_up req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/index")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* terms = Settings.terms_and_conditions database_label language in
    Page.Contact.sign_up terms context flash_fetcher
    |> create_layout req ~active_navigation:"/signup" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let sign_up_create req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let terms_key = Field.(TermsAccepted |> show) in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.remove_empty_values
    ||> HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let result { Pool_context.database_label; query_language; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Logger.req req in
    Utils.Lwt_result.map_error (fun msg ->
      msg, "/signup", [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let* () =
         CCList.assoc ~eq:( = ) terms_key urlencoded
         |> CCList.hd
         |> CCString.equal "true"
         |> Utils.Bool.to_result TermsAndConditionsNotAccepted
         |> Lwt_result.lift
       in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes database_label
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let preferred_language = HttpUtils.browser_language_from_req req in
       let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* create_contact_events =
         let open CCResult.Infix in
         Command.SignUp.(
           decode urlencoded
           >>= handle ~tags ?allowed_email_suffixes tenant preferred_language)
         |> Lwt_result.lift
       in
       let* email_address =
         Sihl.Web.Request.urlencoded Field.(Email |> show) req
         ||> CCOption.to_result ContactSignupInvalidEmail
         >== Pool_user.EmailAddress.create
       in
       let%lwt existing_user =
         Service.User.find_by_email_opt
           ~ctx:(Pool_tenant.to_ctx database_label)
           (Pool_user.EmailAddress.value email_address)
       in
       let* events =
         match existing_user with
         | None -> Lwt_result.return create_contact_events
         | Some user when Service.User.is_admin user -> Lwt_result.return []
         | Some _ ->
           email_address
           |> Contact.find_by_email database_label
           ||> (function
           | Ok contact when contact.Contact.user.Sihl_user.confirmed -> Ok []
           | Ok contact ->
             let open CCResult in
             contact
             |> Command.DeleteUnverified.handle ~tags
             >|= CCFun.flip CCList.append create_contact_events
           | Error _ -> Ok [])
       in
       Utils.Database.with_transaction database_label (fun () ->
         let tags = Logger.req req in
         let%lwt () = Pool_event.handle_events ~tags database_label events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/email-confirmation")
             [ Message.set
                 ~success:[ Pool_common.Message.EmailConfirmationMessage ]
             ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  let tags = Logger.req req in
  let result ({ Pool_context.database_label; query_language; _ } as context) =
    let open Pool_common.Message in
    let%lwt redirect_path =
      let user =
        Pool_context.find_contact context
        |> CCResult.map (fun contact -> contact.Contact.user)
        |> CCOption.of_result
      in
      match user with
      | None -> "/login" |> Lwt.return
      | Some user ->
        let open Pool_context in
        user_of_sihl_user database_label user ||> dashboard_path
    in
    (let* token =
       Sihl.Web.Request.query Field.(show Token) req
       |> CCOption.map Email.Token.create
       |> CCOption.to_result Field.(NotFound Token)
       |> Lwt_result.lift
     in
     let ctx = Pool_tenant.to_ctx database_label in
     let* email =
       Service.Token.read
         ~ctx
         (Email.Token.value token)
         ~k:Field.(Email |> show)
       ||> CCOption.to_result TokenInvalidFormat
       >== Pool_user.EmailAddress.create
       >>= Email.find_unverified_by_address database_label
       |> Lwt_result.map_error (fun _ -> Field.(Invalid Token))
     in
     let* events =
       let open UserCommand in
       let%lwt admin =
         Admin.find
           database_label
           (email |> Email.user_id |> Pool_common.Id.value |> Admin.Id.of_string)
       in
       let%lwt contact = Contact.find database_label (Email.user_id email) in
       let verify_email user =
         VerifyEmail.(handle ~tags user email) |> Lwt_result.lift
       in
       let update_email user =
         UpdateEmail.(handle ~tags user email) |> Lwt_result.lift
       in
       match email |> Email.user_is_confirmed, contact, admin with
       | false, Ok contact, _ -> verify_email (Contact contact)
       | true, Ok contact, _ -> update_email (Contact contact)
       | false, Error _, Ok admin -> verify_email (Admin admin)
       | true, _, Ok admin -> update_email (Admin admin)
       | true, Error _, Error _ | false, Error _, Error _ ->
         Logs.err (fun m ->
           m
             "Impossible email update tried: %s with context: %s"
             ([%show: Email.t] email)
             ([%show: Pool_context.t] context));
         Lwt.return_ok []
     in
     let%lwt () = Pool_event.handle_events ~tags database_label events in
     HttpUtils.(
       redirect_to_with_actions
         (path_with_language query_language redirect_path)
         [ Message.set ~success:[ EmailVerified ] ])
     |> Lwt_result.ok)
    >|- fun msg -> msg, redirect_path
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; language; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* terms = Settings.terms_and_conditions database_label language in
       Page.Contact.terms
         Contact.(contact |> id |> Pool_common.Id.value)
         terms
         context
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms_accept req =
  let result { Pool_context.database_label; query_language; _ } =
    Utils.Lwt_result.map_error (fun msg -> msg, "/login")
    @@
    let open Utils.Lwt_result.Infix in
    let id =
      Pool_common.(
        Sihl.Web.Router.param req Message.Field.(Id |> show) |> Id.of_string)
    in
    let* contact = Contact.find database_label id in
    let tags = Logger.req req in
    let* events =
      Command.AcceptTermsAndConditions.handle ~tags contact |> Lwt_result.lift
    in
    let tags = Logger.req req in
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    HttpUtils.(redirect_to (path_with_language query_language "/dashboard"))
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;
