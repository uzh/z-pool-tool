module Message = Http_utils.Message
module Login = Public_login
module Common = Pool_common

let index req =
  let open Sihl.Web in
  let message = CCOption.bind (Flash.find_alert req) Message.of_string in
  Page.Public.index message () |> Response.of_html |> Lwt.return
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
    Service.Storage.find
      ~ctx:Common.(Utils.pool_to_ctx Database.root)
      ~id:asset_id
  in
  let%lwt content = Service.Storage.download_data_base64 file in
  let mime = file.file.mime in
  let content = content |> Base64.decode_exn in
  Sihl.Web.Response.of_plain_text content
  |> Sihl.Web.Response.set_content_type mime
  |> Lwt.return
;;
