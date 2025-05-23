open Utils.Lwt_result.Infix
open Pool_message
module Response = Http_response

let src = Logs.Src.create "handler.root.announcements"
let active_navigation = "/root/announcements"
let create_layout = General.create_root_layout
let announcement_path = Http_utils.Url.Root.announcement_path
let boolean_fields = Field.[ show ShowToAdmins; show ShowToContacts ]

let announcement_id req =
  Http_utils.get_field_router_param req Field.Announcement |> Announcement.Id.of_string
;;

let text_from_urlencoded urlencoded =
  let open CCOption.Infix in
  let open Pool_common.Language in
  let sys_languages = all in
  sys_languages
  |> CCList.filter_map (fun lang ->
    let field language = Format.asprintf "%s[%s]" Field.(show Text) (show language) in
    CCList.assoc_opt ~eq:CCString.equal (field lang) urlencoded
    >>= CCList.head_opt
    >|= fun text -> lang, text)
;;

let selected_tenants_from_urlencoded req =
  let open CCList in
  let open Pool_tenant in
  let%lwt tenants = find_all () in
  let%lwt selected_tenants =
    Sihl.Web.Request.urlencoded_list Field.(array_key Tenant) req
  in
  tenants
  |> filter_map (fun { id; _ } ->
    if mem (Id.value id) selected_tenants then Some id else None)
  |> Lwt.return
;;

let index req =
  let create_layout (_ : Rock.Request.t) ?active_navigation context children =
    General.create_root_layout ?active_navigation context children |> Lwt_result.ok
  in
  Response.Htmx.index_handler
    ~active_navigation:(announcement_path ())
    ~query:(module Announcement)
    ~create_layout
    req
  @@ fun (Pool_context.{ database_label; _ } as context) query ->
  let%lwt announcements = Announcement.all ~query database_label in
  let open Page.Root.Announcement in
  (if Http_utils.Htmx.is_hx_request req then list else index) context announcements
  |> Lwt_result.return
;;

let form case req =
  let result context =
    let sys_languages = Pool_common.Language.all in
    let%lwt tenants = Pool_tenant.find_all () in
    let* announcement =
      match case with
      | `New -> Lwt_result.return None
      | `Edit ->
        announcement_id req
        |> Announcement.find_admin
        >|- Response.not_found
        >|+ CCOption.return
    in
    Page.Root.Announcement.form context tenants sys_languages ?announcement
    |> create_layout context
    ||> Sihl.Web.Response.of_html
    ||> CCResult.return
  in
  Response.handle ~src req result
;;

let new_form = form `New
let edit = form `Edit

let create req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> Http_utils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let events =
      let open CCResult in
      let open Cqrs_command.Announcement_command.Create in
      let%lwt tenant_ids = selected_tenants_from_urlencoded req in
      urlencoded
      |> Http_utils.format_request_boolean_values boolean_fields
      |> decode
      >>= handle ~tags:Logs.Tag.empty tenant_ids
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (announcement_path ())
        [ Http_utils.Message.set ~success:[ Success.Created Field.Announcement ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let update req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> Http_utils.remove_empty_values
  in
  let id = announcement_id req in
  let result { Pool_context.database_label; user; _ } =
    let* announcement = Announcement.find id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded edit
    @@
    let events =
      let open CCResult in
      let open Cqrs_command.Announcement_command.Update in
      let%lwt tenant_ids = selected_tenants_from_urlencoded req in
      urlencoded
      |> Http_utils.format_request_boolean_values boolean_fields
      |> decode
      >>= handle ~tags:Logs.Tag.empty announcement tenant_ids
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (announcement_path ())
        [ Http_utils.Message.set ~success:[ Success.Updated Field.Announcement ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access
end = struct
  module Command = Cqrs_command.Announcement_command
  module Guardian = Middleware.Guardian

  let announcement_effects =
    Guardian.id_effects Announcement.Id.validate Field.Announcement
  ;;

  let index = Announcement.Access.index |> Guardian.validate_admin_entity ~any_id:true
  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let read = announcement_effects Announcement.Access.read
  let update = announcement_effects Command.Update.effects
  let delete = Guardian.denied
end
