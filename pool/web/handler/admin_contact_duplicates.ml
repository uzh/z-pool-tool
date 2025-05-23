open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Page = Page.Admin.Contact.Duplicates
module Response = Http_response
module Url = HttpUtils.Url

let src = Logs.Src.create "handler.admin.contacts"
let create_layout req = General.create_tenant_layout req
let duplicate_path = HttpUtils.Url.Admin.duplicate_path

let contact_id =
  let open CCFun.Infix in
  HttpUtils.find_id_save (Contact.Id.of_string %> CCResult.return) Field.Contact
  %> CCResult.to_opt
;;

let duplicate_id = HttpUtils.find_id Duplicate_contacts.Id.of_string Field.Duplicate

let index req =
  let contact_id = contact_id req in
  Response.Htmx.index_handler ~query:(module Duplicate_contacts) ~create_layout req
  @@ fun (Pool_context.{ database_label; _ } as context) query ->
  let open Contact in
  let* contact =
    match contact_id with
    | None -> Lwt_result.return None
    | Some id -> find database_label id |> Lwt_result.map Option.some
  in
  let%lwt possible_duplicates =
    match contact with
    | Some contact -> Duplicate_contacts.find_by_contact ~query database_label contact
    | None -> Duplicate_contacts.all ~query database_label
  in
  (if Http_utils.Htmx.is_hx_request req then Page.list else Page.index)
    context
    ?contact
    possible_duplicates
  |> Lwt_result.return
;;

let show req =
  let result ({ Pool_context.database_label; _ } as context) =
    let open Duplicate_contacts in
    let* duplicate =
      duplicate_id req |> find database_label |> Response.not_found_on_error
    in
    Response.bad_request_render_error context
    @@
    let%lwt fields =
      Custom_field.find_by_model database_label Custom_field.Model.Contact
    in
    let get_fields contact =
      contact
      |> Contact.id
      |> Custom_field.find_to_merge_contact database_label
      ||> CCPair.make contact
    in
    let%lwt contact_a = get_fields duplicate.contact_a in
    let%lwt contact_b = get_fields duplicate.contact_b in
    Page.show context fields contact_a contact_b duplicate
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let ignore req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let* duplicate =
      duplicate_id req
      |> Duplicate_contacts.find database_label
      |> Response.not_found_on_error
    in
    Response.bad_request_on_error show
    @@
    let* () =
      let open Cqrs_command.Duplicate_contacts_command.Ignore in
      handle ~tags duplicate
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label user
    in
    Http_utils.redirect_to_with_actions
      (duplicate_path ())
      [ Http_utils.Message.set ~success:Pool_message.[ Success.Updated Field.Duplicate ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let merge req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> Http_utils.remove_empty_values
    in
    let* duplicate =
      duplicate_id req |> Duplicate_contacts.find database_label >|- Response.not_found
    in
    Response.bad_request_on_error ~urlencoded show
    @@
    let open Duplicate_contacts in
    let%lwt fields =
      Custom_field.find_by_model database_label Custom_field.Model.Contact
    in
    let get_fields contact =
      contact |> Contact.id |> Custom_field.find_to_merge_contact database_label
    in
    let%lwt fields_a = get_fields duplicate.contact_a in
    let%lwt fields_b = get_fields duplicate.contact_b in
    let* data =
      let open Cqrs_command.Duplicate_contacts_command.Merge in
      handle ~tags urlencoded duplicate fields (fields_a, fields_b) |> Lwt_result.lift
    in
    let redirect () =
      Http_utils.redirect_to_with_actions
        (duplicate_path ())
        [ Http_utils.Message.set ~success:Pool_message.[ Success.Updated Field.Duplicate ]
        ]
    in
    data |> merge ?user_uuid:(Pool_event.user_uuid user) database_label |>> redirect
  in
  Response.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access

  val ignore : Rock.Middleware.t
  val merge : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let duplicate_effect =
    Guardian.id_effects Duplicate_contacts.Id.validate Field.Duplicate
  ;;

  let index =
    Duplicate_contacts.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let read = duplicate_effect Duplicate_contacts.Access.read
  let ignore = duplicate_effect Duplicate_contacts.Access.update
  let merge = duplicate_effect Duplicate_contacts.Access.update
end
