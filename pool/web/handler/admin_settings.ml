module HttpUtils = Http_utils
module Message = HttpUtils.Message

let show req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
    in
    let csrf = HttpUtils.find_csrf req in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* languages = Settings.find_languages tenant_db () in
    let* email_suffixes = Settings.find_email_suffixes tenant_db () in
    let* contact_email = Settings.find_contact_email tenant_db () in
    Page.Admin.Settings.show
      csrf
      languages
      email_suffixes
      contact_email
      message
      ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;

let update_settings urlencoded handler req =
  let open Utils.Lwt_result.Infix in
  let redirect_path = "/admin/settings" in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, redirect_path)
    in
    let events () =
      let command_handler = function
        | `UpdateTenantLanguages ->
          fun urlencoded ->
            urlencoded
            |> Cqrs_command.Settings_command.UpdateLanguages.handle
            |> Lwt_result.lift
        | `UpdateTenantEmailSuffixes ->
          fun urlencoded ->
            urlencoded
            |> Cqrs_command.Settings_command.UpdateEmailSuffixes.handle
            |> Lwt_result.lift
        | `CreateTenantEmailSuffix ->
          fun urlencoded ->
            let* email_suffixes = Settings.find_email_suffixes tenant_db () in
            let open CCResult.Infix in
            urlencoded
            |> Cqrs_command.Settings_command.CreateEmailSuffixes.decode
            |> CCResult.map_err Utils.handle_conformist_error
            >>= Cqrs_command.Settings_command.CreateEmailSuffixes.handle
                  email_suffixes
            |> Lwt_result.lift
      in
      urlencoded
      |> command_handler handler
      |> Lwt_result.map_err (fun err -> err, redirect_path)
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event tenant_db) in
    let%lwt return_to_settings =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ "Settings were updated successfully." ] ]
    in
    () |> events |>> handle >|= CCFun.const return_to_settings
  in
  result |> HttpUtils.extract_happy_path
;;

let update_tenant_languages req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let urlencoded =
    urlencoded
    |> HttpUtils.format_request_boolean_values (Settings.Language.all_codes ())
  in
  update_settings urlencoded `UpdateTenantLanguages req
;;

let update_tenant_email_suffixes req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  update_settings urlencoded `UpdateTenantEmailSuffixes req
;;

let create_tenant_email_suffix req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  update_settings urlencoded `CreateTenantEmailSuffix req
;;
