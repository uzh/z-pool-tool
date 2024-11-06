module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field
module Page = Page.Admin.Contact.Duplicates
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.admin.contacts"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let contact_duplicate_path = HttpUtils.Url.Admin.contact_duplicate_path
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact

let duplicate_id =
  HttpUtils.find_id Duplicate_contacts.Id.of_string Field.Duplicate
;;

let index req =
  let contact_id = contact_id req in
  let error_path =
    Format.asprintf "/admin/contacts/%s" (Contact.Id.value contact_id)
  in
  Http_utils.Htmx.handler
    ~error_path
    ~query:(module Duplicate_contacts)
    ~create_layout
    req
  @@ fun (Pool_context.{ database_label; _ } as context) query ->
  let open Contact in
  let* contact = find database_label contact_id in
  let%lwt possible_duplicates =
    Duplicate_contacts.find_by_contact ~query database_label contact
  in
  (if Http_utils.Htmx.is_hx_request req then Page.list else Page.index)
    context
    contact
    possible_duplicates
  |> Lwt_result.return
;;

let show req =
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Lwt_result.map_error (fun err -> err, contact_duplicate_path contact_id ())
    @@
    let open Duplicate_contacts in
    let with_fields contact =
      let open Custom_field in
      Contact.id contact
      |> find_all_by_contact database_label user
      ||> (fun (groups, ungrouped) ->
            groups
            |> CCList.fold_left
                 (fun acc group -> group.Group.Public.fields @ acc)
                 ungrouped)
      ||> CCPair.make contact
    in
    let* duplicate = duplicate_id req |> find database_label in
    let%lwt contact_a = with_fields duplicate.contact_a in
    let%lwt contact_b = with_fields duplicate.contact_b in
    Page.show context contact_a contact_b duplicate
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let ignore req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let id = duplicate_id req in
  let duplicate_path = contact_duplicate_path contact_id ~id () in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, duplicate_path)
    @@
    let* duplicate = Duplicate_contacts.find database_label id in
    let* () =
      let open Cqrs_command.Duplicate_contacts_command.Ignore in
      handle ~tags:Logs.Tag.empty duplicate
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label
    in
    Http_utils.redirect_to_with_actions
      duplicate_path
      [ Http_utils.Message.set
          ~success:Pool_message.[ Success.Updated Field.Duplicate ]
      ]
    |> Lwt_result.ok
  in
  result |> Http_utils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access
end = struct
  include Helpers.Access
end
