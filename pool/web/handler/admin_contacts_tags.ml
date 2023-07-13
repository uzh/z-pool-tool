open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.contacts.tags"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact

let assign_tag req =
  let redirect err =
    HttpUtils.Htmx.htmx_redirect
      (contact_id req
       |> Contact.Id.value
       |> Format.asprintf "/admin/contacts/%s")
      ~actions:[ Message.set ~error:[ err ] ]
      ()
  in
  let%lwt _urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; _ } =
    let%lwt contact =
      HttpUtils.get_field_router_param req Field.Contact
      |> Pool_common.Id.of_string
      |> Contact.find database_label
    in
    match contact with
    | Ok contact -> Helpers.PartialUpdate.update ~contact req
    | Error err -> redirect err
  in
  let context = req |> Pool_context.find in
  match context with
  | Ok context -> result context
  | Error err -> redirect err
;;
