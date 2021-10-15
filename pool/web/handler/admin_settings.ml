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
    Page.Admin.Settings.show csrf languages email_suffixes message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;
