open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field
module Response = Http_response
module View = Page.Admin.OrganisationalUnit

let field = Field.OrganisationalUnit
let ou_path = HttpUtils.Url.Admin.organisational_unit_path
let src = Logs.Src.create "handler.admin.organisational_units"
let create_layout req = General.create_tenant_layout req

let id req =
  Sihl.Web.Router.param req @@ Field.show Field.OrganisationalUnit
  |> Organisational_unit.Id.of_string
;;

let database_label_of_req req =
  let open CCResult in
  Pool_context.(req |> find >|= fun { database_label; _ } -> database_label)
;;

let index req =
  let active_navigation = ou_path () in
  Response.Htmx.index_handler
    ~active_navigation
    ~create_layout
    ~query:(module Organisational_unit)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt organisational_unit_list, query =
    Organisational_unit.find_by query database_label
  in
  (if HttpUtils.Htmx.is_hx_request req then View.list else View.index)
    context
    organisational_unit_list
    query
  |> Lwt_result.return
;;

let show action req =
  let result ({ Pool_context.database_label; _ } as context) =
    let* ou =
      req |> id |> Organisational_unit.find database_label >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let html =
      match action with
      | `New -> View.create context
      | `Edit -> View.detail context ou
    in
    html |> create_layout req context >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_form = show `New
let edit = show `Edit

let create req =
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Organisational_unit_command in
      Create.(urlencoded |> decode |> Lwt_result.lift >== handle ~tags)
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (ou_path ())
        [ HttpUtils.Message.set ~success:[ Pool_message.Success.Created field ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let update req =
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let id = id req in
  let result { Pool_context.database_label; user; _ } =
    let* ou = Organisational_unit.find database_label id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded edit
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Organisational_unit_command in
      Update.(urlencoded |> decode |> Lwt_result.lift >== handle ~tags ou)
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (ou_path ~id ~suffix:"edit" ())
        [ HttpUtils.Message.set ~success:[ Pool_message.Success.Updated field ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let changelog req =
  let ou_id = id req in
  let url =
    HttpUtils.Url.Admin.organisational_unit_path ~suffix:"changelog" ~id:ou_id ()
  in
  let open Organisational_unit in
  Helpers.Changelog.htmx_handler ~url (Id.to_common ou_id) req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Organisational_unit_command
  module Guardian = Middleware.Guardian

  let organisational_units =
    Guardian.id_effects Organisational_unit.Id.validate Field.OrganisationalUnit
  ;;

  let index =
    Guardian.validate_admin_entity ~any_id:true Organisational_unit.Guard.Access.index
  ;;

  let create = Guardian.validate_admin_entity Command.Create.effects
  let update = organisational_units Command.Update.effects
end
