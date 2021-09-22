module HttpUtils = Http_utils
module Message = HttpUtils.Message

let handle_conformist_error (err : Conformist.error list) =
  String.concat
    "\n"
    (List.map (fun (m, _, k) -> Format.asprintf "%s: %s" m k) err)
;;

let tenants req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    let* tenant_list = Tenant.find_all () in
    Page.Root.Tenant.list csrf ~tenant_list ~message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;

let create _ =
  let open Utils.Lwt_result.Infix in
  let error_path = "/root/tenants" in
  let events () =
    Lwt_result.lift
    @@
    let open CCResult.Infix in
    (* TODO exchange with "urlencoded" *)
    [ "title", [ "title" ]
    ; "description", [ "description" ]
    ; "url", [ "url" ]
    ; "database_url", [ "database" ]
    ; "smtp_auth_server", [ "smtp.uzh.ch" ]
    ; "smtp_auth_port", [ "587" ]
    ; "smtp_auth_username", [ "engineering@econ.uzh.ch" ]
    ; "smtp_auth_authentication_method", [ "LOGIN" ]
    ; "smtp_auth_protocol", [ "SSL/TLS" ]
    ; "styles", [ "custom_stylesheet.css" ]
    ; "icon", [ "some icon" ]
    ; "logos", [ "some logos" ]
    ; "partner_logos", [ "some partner" ]
    ; "default_language", [ "EN" ]
    ]
    |> Cqrs_command.Tenant_command.AddTenant.decode
    |> CCResult.map_err handle_conformist_error
    >>= Cqrs_command.Tenant_command.AddTenant.handle
    |> CCResult.map_err (fun err -> err, error_path)
  in
  let handle events =
    let ( let* ) = Lwt.bind in
    let* _ =
      Lwt_list.map_s (fun event -> Pool_event.handle_event event) events
    in
    Lwt.return_ok ()
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Tenant was successfully created." ] ]
  in
  ()
  |> events
  >|= handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

(* let[@warning "-27"] create_without_event req = let open
   Utils.Lwt_result.Infix in let open Lwt_result.Syntax in let lift =
   Lwt_result.lift in let create () = Lwt_result.map_err (fun err -> err,
   "/root/tenants") @@ let open Tenant in let* title = Title.create "title" |>
   lift in let* description = Description.create "description" |> lift in let*
   url = Url.create "url" |> lift in let* database = Database.create "database"
   |> lift in let* smtp_auth_server = SmtpAuth.Server.create "smtp.uzh.ch" |>
   lift in let* smtp_auth_port = SmtpAuth.Port.create "587" |> lift in let*
   smtp_auth_username = SmtpAuth.Username.create "engineering@econ.uzh.ch" |>
   lift in let* smtp_auth_authentication_method =
   SmtpAuth.AuthenticationMethod.create "LOGIN" |> lift in let*
   smtp_auth_protocol = SmtpAuth.Protocol.create "SSL/TLS" |> lift in let*
   smtp_auth = SmtpAuth.create smtp_auth_server smtp_auth_port
   smtp_auth_username smtp_auth_authentication_method smtp_auth_protocol |> lift
   in let* styles = Styles.create "custom_stylesheet.css" |> lift in let* icon =
   Icon.create "some icon" |> lift in let* logos = Logos.create "some logos" |>
   lift in let* partner_logos = PartnerLogo.create "some partner" |> lift in
   let* default_language = Settings.Language.of_string "EN" |> lift in Ok
   (Tenant.create title description url database smtp_auth styles icon logos
   partner_logos default_language) |> lift in let handle (tenant : Tenant.t) =
   let* _ = Tenant.insert tenant in Lwt.return_ok () in let return_to_overview =
   Http_utils.redirect_to_with_actions "/root/tenants" [ Message.set ~success:[
   "Tenant was successfully created." ] ] in () |> create >|= handle |>>
   CCFun.const return_to_overview >|> HttpUtils.extract_happy_path ;; *)
