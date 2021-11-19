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
      |> Utils.bool_to_result Pool_common.Message.TermsAndConditionsNotAccepted
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
      if CCList.length suffixes > 0 then Some suffixes else None
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
