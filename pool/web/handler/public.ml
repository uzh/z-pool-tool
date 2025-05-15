open Pool_message
module Message = Http_utils.Message
module Login = Public_login
module Import = Public_import
module Common = Pool_common
module Database = Database
module Response = Http_response

let src = Logs.Src.create "handler.public"
let create_layout req = General.create_tenant_layout req
let root_label = Database.Pool.Root.label

let root_redirect req =
  let open Http_utils in
  (fun path -> retain_url_params req path |> Uri.to_string |> redirect_to)
  @@
  match Http_utils.is_req_from_root_host req with
  | true -> "/root"
  | false -> "/index"
;;

let index req =
  if Http_utils.is_req_from_root_host req
  then Http_utils.redirect_to "/root"
  else (
    let result ({ Pool_context.database_label; language; _ } as context) =
      let open Utils.Lwt_result.Infix in
      Response.bad_request_render_error context
      @@ let* tenant = Pool_tenant.find_by_label database_label in
         let%lwt welcome_text =
           I18n.find_by_key database_label I18n.Key.WelcomeText language
         in
         let%lwt signup_cta =
           I18n.find_by_key database_label I18n.Key.SignUpCTA language
         in
         Page.Public.index tenant context welcome_text signup_cta
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
    in
    Response.handle ~src req result)
;;

let index_css req =
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let* styles =
      let open Pool_context.Tenant in
      find req
      |> Lwt_result.lift
      >== fun { tenant; _ } ->
      tenant.Pool_tenant.styles |> CCOption.to_result (Error.NotFound Field.Styles)
    in
    let* file =
      Http_utils.File.get_storage_file
        ~tags
        root_label
        (styles |> Pool_tenant.Styles.id |> Common.Id.value)
    in
    let%lwt content =
      Storage.download_data_base64 root_label file ||> Base64.decode_exn
    in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type
         (styles |> Pool_tenant.Styles.mime_type |> Common.File.Mime.to_string)
    |> Lwt.return_ok
  in
  match result with
  | Ok res -> Lwt.return res
  | Error error ->
    let (_ : Error.t) =
      Common.Utils.with_log_error ~src:(Logs.Src.create "handler.public.style") error
    in
    Lwt.return Response.empty_not_found
;;

let email_confirmation_note req =
  let open Common.I18n in
  General.note ~title:EmailConfirmationTitle ~body:EmailConfirmationNote req
;;

(* TODO: Does that work as expected? *)
let not_found req =
  let error (_ : Pool_context.t) = Response.generic_not_found |> Lwt_result.fail in
  Response.handle ~src req error
;;

(* TODO: Simplify? *)
let denied req =
  let open Utils.Lwt_result.Infix in
  let open Common in
  match req |> Pool_context.find with
  | Error (_ : Pool_message.Error.t) -> Utils.failwith (Error.NotFound Field.Context)
  | Ok ({ Pool_context.database_label; language; _ } as context) ->
    let tenant = Pool_context.Tenant.find req in
    let html =
      Page.Utils.error_page_terminatory
        ~lang:language
        Error.AccessDenied
        Error.AccessDeniedMessage
        ()
    in
    (match Pool_context.is_from_root context, tenant with
     | false, Ok tenant -> Layout.Tenant.create context tenant html
     | false, Error _ | true, Ok _ | true, Error _ ->
       let context =
         let open Pool_context in
         let csrf = Sihl.Web.Csrf.find_exn req in
         create ([], Pool_common.Language.En, database_label, None, csrf, Guest, [], [])
       in
       Layout.Root.create context html)
    ||> Sihl.Web.Response.of_html
;;

let asset req =
  let open Utils.Lwt_result.Infix in
  let open Sihl.Contract.Storage in
  let%lwt response =
    let tags = Pool_context.Logger.Tags.req req in
    let asset_id = Sihl.Web.Router.param req Field.(Id |> show) in
    let* file = Http_utils.File.get_storage_file ~tags root_label asset_id in
    let%lwt content = Storage.download_data_base64 root_label file in
    let mime = file.file.mime in
    let content = content |> Base64.decode_exn in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type mime
    |> Lwt.return_ok
  in
  response
  |> function
  | Ok response -> Lwt.return response
  | Error error ->
    let (_ : Error.t) =
      Common.Utils.with_log_error ~src:(Logs.Src.create "handler.public.asset") error
    in
    Lwt.return Response.empty_not_found
;;

let error req =
  let query_lang = Http_utils.find_query_lang req in
  let error_page (title, note) =
    Page.Utils.error_page_terminatory ?lang:query_lang title note ()
  in
  Error.(TerminatoryRootErrorTitle, TerminatoryRootError)
  |> error_page
  |> Layout.Error.create
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let credits req =
  let result ({ Pool_context.language; database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let open Utils.Lwt_result.Infix in
    let%lwt html =
      I18n.find_by_key database_label I18n.Key.CreditsText language
      ||> Page.Utils.i18n_page
    in
    html |> create_layout req context >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let privacy_policy req =
  let result ({ Pool_context.language; query_parameters; database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let redirect_path = Http_utils.url_with_field_params query_parameters "/" in
    let open Utils.Lwt_result.Infix in
    let%lwt policy =
      I18n.find_by_key_opt database_label I18n.Key.PrivacyPolicy language
    in
    match policy with
    | None -> Http_utils.redirect_to redirect_path ||> CCResult.return
    | Some policy ->
      policy
      |> Page.Utils.i18n_page
      |> create_layout req context
      >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let terms_and_conditions req =
  let result ({ Pool_context.language; database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let open Utils.Lwt_result.Infix in
    let%lwt terms =
      I18n.find_by_key database_label I18n.Key.TermsAndConditions language
    in
    let%lwt terms_last_updated = I18n.terms_and_conditions_last_updated database_label in
    Page.Public.terms_and_conditions language terms terms_last_updated
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let hide_announcement req =
  let open Http_utils in
  let result { Pool_context.user; database_label; _ } =
    let open Utils.Lwt_result.Infix in
    let open Announcement in
    let* announcement =
      find_id Id.validate Field.Announcement req
      |> Lwt_result.lift
      >>= find_of_tenant database_label
    in
    let* () =
      Cqrs_command.Announcement_command.Hide.handle (user, announcement)
      |> Lwt_result.lift
      |>> Pool_event.handle_events Database.Pool.Root.label user
    in
    Tyxml.Html.txt "" |> Response.Htmx.of_html |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;
