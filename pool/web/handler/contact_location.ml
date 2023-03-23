module HttpUtils = Http_utils
module Field = Pool_common.Message.Field

let create_layout = Contact_general.create_layout

let id req field encode =
  Sihl.Web.Router.param req @@ Field.show field |> encode
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let id = id req Field.Location Pool_location.Id.of_string in
    let* location = Pool_location.find database_label id in
    Page.Contact.Location.show context location
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let asset req =
  let open Utils.Lwt_result.Infix in
  let open Sihl.Contract.Storage in
  let open Pool_location in
  let id = id req Field.File Pool_common.Id.of_string in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, "/not-found")
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let ctx = Pool_database.to_ctx database_label in
    let* file =
      find_location_file database_label id
      >>= fun { Mapping.file; _ } ->
      file.Pool_common.File.id
      |> Pool_common.Id.value
      |> HttpUtils.File.get_storage_file ~tags database_label
    in
    let%lwt content = Service.Storage.download_data_base64 ~ctx file in
    let mime = file.file.mime in
    let content = content |> Base64.decode_exn in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type mime
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;
