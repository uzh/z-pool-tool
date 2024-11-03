module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field
module Page = Page.Admin.Contact.Duplicates
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.admin.contacts"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact

let duplicate_id =
  HttpUtils.find_id Duplicate_contacts.Id.of_string Field.Duplicate
;;

let index req =
  let contact_id = contact_id req in
  let error_path =
    Format.asprintf "/admin/contacts/%s" (Contact.Id.value contact_id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Contact in
    let* contact =
      HttpUtils.get_field_router_param req Field.Contact
      |> Id.of_string
      |> find database_label
    in
    let%lwt possible_duplicates =
      Duplicate_contacts.find_by_contact database_label contact
    in
    Page.index contact possible_duplicates
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let show req =
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Lwt_result.map_error (fun err ->
      err, HttpUtils.Url.Admin.contact_duplicate_path contact_id ())
    @@
    let custom_fields contact_id =
      let open Custom_field in
      find_all_by_contact database_label user contact_id
      ||> fun (groups, ungrouped) ->
      groups
      |> CCList.fold_left
           (fun acc group -> group.Group.Public.fields @ acc)
           ungrouped
    in
    let* contact =
      Contact.find database_label contact_id
      |>> fun contact ->
      custom_fields (Contact.id contact) ||> CCPair.make contact
    in
    let* possible_duplicate =
      let open Duplicate_contacts in
      duplicate_id req
      |> find database_label
      |>> fun duplicate ->
      custom_fields (Contact.id duplicate.contact) ||> CCPair.make duplicate
    in
    Page.show context contact possible_duplicate
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

module Access : sig
  include module type of Helpers.Access
end = struct
  include Helpers.Access
end
