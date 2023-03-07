module Message = Http_utils.Message
module Login = Public_login
module Common = Pool_common
module Database = Pool_database

let create_layout req = General.create_tenant_layout req

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
    let result
      ({ Pool_context.database_label; language; query_language; _ } as context)
      =
      let open Utils.Lwt_result.Infix in
      let error_path = Http_utils.path_with_language query_language "/error" in
      Utils.Lwt_result.map_error (fun err -> err, error_path)
      @@ let* tenant = Pool_tenant.find_by_label database_label in
         let* welcome_text =
           I18n.find_by_key database_label I18n.Key.WelcomeText language
         in
         Page.Public.index tenant context welcome_text
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
    in
    result |> Http_utils.extract_happy_path req)
;;

let index_css req =
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let* { Pool_context.database_label; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let* styles = Pool_tenant.find_styles database_label in
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
  let result ({ Pool_context.language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let txt_to_string m = Common.Utils.text_to_string language m in
    Common.I18n.(
      Page.Utils.note
        (txt_to_string EmailConfirmationTitle)
        (txt_to_string EmailConfirmationNote))
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path req
;;

let not_found req =
  let result
    ({ Pool_context.language; query_language; database_label; _ } as context)
    =
    let open Utils.Lwt_result.Infix in
    let html = Page.Utils.error_page_not_found language () in
    match Http_utils.is_req_from_root_host req with
    | true ->
      General.create_root_layout context html
      |> Sihl.Web.Response.of_html
      |> Lwt_result.return
    | false ->
      Utils.Lwt_result.map_error (fun err ->
        err, Http_utils.path_with_language query_language "/error")
      @@ let* tenant = Pool_tenant.find_by_label database_label in
         let%lwt tenant_languages = Settings.find_languages database_label in
         let req =
           Pool_context.Tenant.set
             req
             (Pool_context.Tenant.create tenant tenant_languages)
         in
         html |> create_layout req context >|+ Sihl.Web.Response.of_html
  in
  result
  |> Http_utils.extract_happy_path req
  |> Lwt.map @@ Opium.Response.set_status `Not_found
;;

let denied req =
  let context = req |> Pool_context.find in
  match context with
  | Error (_ : Pool_common.Message.error) -> failwith ""
  | Ok ({ Pool_context.language; query_language; message; user; _ } as context)
    ->
    let tenant = Pool_context.Tenant.find req in
    let html =
      Page.Utils.error_page_terminatory
        ~lang:language
        Pool_common.Message.AccessDenied
        Pool_common.Message.AccessDeniedMessage
        ()
    in
    (match Pool_context.is_from_root context, tenant with
     | false, Ok tenant ->
       Page.Layout.Tenant.create_layout
         html
         tenant
         user
         message
         language
         query_language
         None
     | false, Error _ | true, Ok _ | true, Error _ ->
       Page.Layout.Root.create_layout Pool_context.Guest None html)
    |> Sihl.Web.Response.of_html
    |> Lwt.return
;;

let asset req =
  let open Sihl.Contract.Storage in
  let asset_id = Sihl.Web.Router.param req Common.Message.Field.(Id |> show) in
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
  let query_lang = Http_utils.find_query_lang req in
  let error_page (title, note) =
    Page.Utils.error_page_terminatory ?lang:query_lang title note ()
  in
  (Common.Message.TerminatoryRootErrorTitle, Common.Message.TerminatoryRootError)
  |> error_page
  |> Page.Layout.create_error_layout
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;
