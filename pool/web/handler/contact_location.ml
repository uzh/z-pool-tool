module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let id =
      let open Pool_common.Message.Field in
      HttpUtils.find_id Pool_location.Id.of_string Location req
    in
    let* location = Pool_location.find database_label id in
    Page.Contact.Location.show context location
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
