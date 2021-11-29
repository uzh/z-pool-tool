module Message = Http_utils.Message
module Login = Public_login
module Common = Pool_common

let index req =
  if Http_utils.is_req_from_root_host req
  then Http_utils.redirect_to "/root"
  else (
    let%lwt result =
      let open Lwt_result.Syntax in
      let message =
        CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
      in
      let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
      let* tenant = Tenant.find_by_label tenant_db in
      Page.Public.index tenant message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
    in
    result
    |> CCResult.map_err (fun err -> err, "/")
    |> Http_utils.extract_happy_path)
;;

let index_css req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* styles = Tenant.find_styles tenant_db in
    let%lwt file =
      Service.Storage.find
        ~ctx:(Pool_common.Utils.pool_to_ctx Pool_common.Database.root)
        (styles |> Tenant.Styles.id |> Pool_common.Id.value)
    in
    let%lwt content =
      Service.Storage.download_data_base64 file ||> Base64.decode_exn
    in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type
         (styles |> Tenant.Styles.mime_type |> Pool_common.File.Mime.to_string)
    |> Lwt.return_ok
  in
  match result with
  | Ok res -> Lwt.return res
  | Error _ ->
    Lwt.return
      (Sihl.Web.Response.set_content_type
         "text/css"
         (Sihl.Web.Response.of_plain_text ""))
;;

let email_confirmation_note req =
  let open Sihl.Web in
  let message = CCOption.bind (Flash.find_alert req) Message.of_string in
  let html =
    Common.Message.I18n.EmailConfirmation.(Page.Utils.note title note)
  in
  message |> html |> Response.of_html |> Lwt.return
;;

let not_found _ =
  let html = Page.Utils.error_page_not_found () in
  Sihl.Web.Response.of_html html |> Lwt.return
;;

let asset req =
  let open Sihl.Contract.Storage in
  let asset_id = Sihl.Web.Router.param req "id" in
  let%lwt file =
    Service.Storage.find ~ctx:Common.(Utils.pool_to_ctx Database.root) asset_id
  in
  let%lwt content = Service.Storage.download_data_base64 file in
  let mime = file.file.mime in
  let content = content |> Base64.decode_exn in
  Sihl.Web.Response.of_plain_text content
  |> Sihl.Web.Response.set_content_type mime
  |> Lwt.return
;;
