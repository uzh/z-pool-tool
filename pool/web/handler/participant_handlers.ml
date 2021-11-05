module Command = Cqrs_command.Participant_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Email = Common_user.Email

let dashboard req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
    in
    Page.Participant.dashboard message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> Http_utils.extract_happy_path
;;

let sign_up req =
  let csrf = HttpUtils.find_csrf req in
  let message =
    Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
  in
  let go = CCFun.flip Sihl.Web.Flash.find req in
  let channels = Participant.RecruitmentChannel.all () in
  let email = go "email" in
  let firstname = go "firstname" in
  let lastname = go "lastname" in
  let recruitment_channel = go "recruitment_channel" in
  (* TODO [timhub]: Implement Terms as setting *)
  let%lwt terms = Settings.terms_and_conditions in
  let html =
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
  in
  Sihl.Web.Response.of_html html |> Lwt.return
;;

let sign_up_create req =
  let terms_key = "_terms_accepted" in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    |> Lwt.map @@ HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* () =
      List.assoc terms_key urlencoded
      |> CCList.hd
      |> CCString.equal "true"
      |> function
      | true -> Lwt.return_ok ()
      | false -> Lwt.return_error "Terms and conditions not accepted"
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* () =
      let open Utils.Lwt_result.Infix in
      Sihl.Web.Request.urlencoded "email" req
      |> Lwt.map
         @@ CCOpt.to_result "Please provide a valid and unused email address."
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let* allowed_email_suffixes =
      let open Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db ()
      >|= Settings.email_suffixes
      |> Lwt_result.map (fun suffixes ->
             if CCList.length suffixes > 0 then Some suffixes else None)
    in
    let* events =
      let open CCResult.Infix in
      Command.SignUp.decode urlencoded
      |> CCResult.map_err Utils.handle_conformist_error
      >>= Command.SignUp.handle ?allowed_email_suffixes
      |> Lwt_result.lift
    in
    let%lwt run =
      Utils.Database.with_transaction tenant_db (fun () ->
          let%lwt () = Pool_event.handle_events tenant_db events in
          HttpUtils.redirect_to_with_actions
            "/participant/email-confirmation"
            [ Message.set
                ~success:
                  [ "Successfully created. An email has been sent to your \
                     email address for verification."
                  ]
            ])
    in
    Lwt.return_ok run
  in
  result
  |> CCResult.map_err (fun msg ->
         ( msg
         , "/participant/signup"
         , [ HttpUtils.urlencoded_to_flash urlencoded
           ; Message.set ~error:[ msg ]
           ] ))
  |> HttpUtils.extract_happy_path_with_actions
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  (let open Lwt_result.Syntax in
  let* token =
    Sihl.Web.Request.query "token" req
    |> CCOpt.map Email.Token.create
    |> CCOpt.to_result "No activation token found!"
    |> Lwt_result.lift
  in
  let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
  let ctx = [ "pool", Pool_common.Database.Label.value tenant_db ] in
  let* email =
    Service.Token.read ~ctx (Email.Token.value token) ~k:"email"
    |> Lwt.map (CCOpt.to_result "Invalid token format!")
    |> CCFun.flip Lwt_result.bind_result Email.Address.create
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
    [ Message.set ~success:[ "Email successfully verified." ] ]
  |> Lwt_result.ok)
  |> Lwt_result.map_err (fun msg -> msg, "/login")
  >|> HttpUtils.extract_happy_path
;;

let terms req =
  CCFun.flip Lwt.bind HttpUtils.extract_happy_path
  @@
  let open Lwt_result.Syntax in
  let open Sihl.Web in
  let csrf = HttpUtils.find_csrf req in
  let message = CCOpt.bind (Flash.find_alert req) Message.of_string in
  let* user =
    Lwt.map
      (CCOpt.to_result ("User could not be found", "/login"))
      (General.user_from_session req)
  in
  (* TODO [timhub]: Implement Terms as setting *)
  let%lwt terms = Settings.terms_and_conditions in
  Page.Participant.terms csrf message user.Sihl_user.id terms ()
  |> Response.of_html
  |> Lwt.return_ok
;;

let terms_accept req =
  let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* participant = Participant.find tenant_db id in
    let* events =
      Command.AcceptTermsAndConditions.handle participant |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    HttpUtils.redirect_to "/participant/dashboard" |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun msg -> msg, "/login")
  |> HttpUtils.extract_happy_path
;;
