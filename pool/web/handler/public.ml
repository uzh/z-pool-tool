module Message = Http_utils.Message
module Login = Public_login
module Common = Pool_common
module Database = Pool_database

let root_redirect req =
  Http_utils.redirect_to
  @@
  match Http_utils.is_req_from_root_host req with
  | true -> "/root"
  | false -> "/index"
;;

let index req =
  if Http_utils.is_req_from_root_host req
  then Http_utils.redirect_to "/root"
  else (
    let result context =
      let open Lwt_result.Syntax in
      let query_lang = context.Pool_context.query_language in
      let error_path = Http_utils.path_with_language query_lang "/error" in
      let message =
        CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
      in
      let tenant_db = context.Pool_context.tenant_db in
      let* tenant =
        Pool_tenant.find_by_label tenant_db
        |> Lwt_result.map_err (fun err -> err, error_path)
      in
      Page.Public.index tenant message context
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
    in
    result |> Http_utils.extract_happy_path req)
;;

let index_css req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* context = Pool_context.find req |> Lwt_result.lift in
    let tenant_db = context.Pool_context.tenant_db in
    let* styles = Pool_tenant.find_styles tenant_db in
    let%lwt file =
      Service.Storage.find
        ~ctx:(Pool_tenant.to_ctx Database.root)
        (styles |> Pool_tenant.Styles.id |> Pool_common.Id.value)
    in
    let%lwt content =
      Service.Storage.download_data_base64 file ||> Base64.decode_exn
    in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type
         (styles
         |> Pool_tenant.Styles.mime_type
         |> Pool_common.File.Mime.to_string)
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
  let result context =
    Lwt_result.map_err (fun err -> err, "/")
    @@
    let language = context.Pool_context.language in
    let txt_to_string m = Common.Utils.text_to_string language m in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let html =
      Common.I18n.(
        Page.Utils.note
          language
          (txt_to_string EmailConfirmationTitle)
          (txt_to_string EmailConfirmationNote))
    in
    message |> html |> Sihl.Web.Response.of_html |> Lwt.return_ok
  in
  result |> Http_utils.extract_happy_path req
;;

let not_found req =
  let result context =
    let query_lang = context.Pool_context.query_language in
    Lwt_result.map_err (fun err ->
        err, Http_utils.path_with_language query_lang "/error")
    @@
    let language = context.Pool_context.language in
    let html = Page.Utils.error_page_not_found language () in
    Sihl.Web.Response.of_html html |> Lwt.return_ok
  in
  result |> Http_utils.extract_happy_path req
;;

let asset req =
  let open Sihl.Contract.Storage in
  let asset_id = Sihl.Web.Router.param req "id" in
  let%lwt file =
    Service.Storage.find ~ctx:(Pool_tenant.to_ctx Database.root) asset_id
  in
  let%lwt content = Service.Storage.download_data_base64 file in
  let mime = file.file.mime in
  let content = content |> Base64.decode_exn in
  Sihl.Web.Response.of_plain_text content
  |> Sihl.Web.Response.set_content_type mime
  |> Lwt.return
;;

let error req =
  (* TODO[timhub]: do not use context. This is the error path of the context
     middleware *)
  let%lwt tenant_error =
    let open Lwt_result.Syntax in
    let* context = Pool_context.find req |> Lwt_result.lift in
    let tenant_db = context.Pool_context.tenant_db in
    let* _ = Pool_tenant.find_by_label tenant_db in
    Ok
      ( Common.Message.TerminatoryTenantErrorTitle
      , Common.Message.TerminatoryTenantError )
    |> Lwt.return
  in
  let root_error =
    ( Common.Message.TerminatoryRootErrorTitle
    , Common.Message.TerminatoryRootError )
  in
  let error_page (title, note) =
    Page.Utils.error_page_terminatory title note ()
  in
  (match tenant_error with
  | Ok tenant_error -> tenant_error
  | Error _ -> root_error)
  |> error_page
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;
