open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let field = Field.Tag
let base_path = "/admin/settings/tags"
let src = Logs.Src.create "handler.admin.settings_tags"
let active_navigation = base_path
let id req = Sihl.Web.Router.param req @@ Field.show field |> Tags.Id.of_string

let index req =
  Response.Htmx.index_handler
    ~active_navigation
    ~query:(module Tags)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt tags, query = Tags.find_by ~query database_label in
  let open Page.Admin.Settings.Tags in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context tags query
  |> Lwt_result.return
;;

let new_form req =
  let result context =
    Page.Admin.Settings.Tags.new_form context
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    |> Response.bad_request_render_error context
  in
  Response.handle ~src req result
;;

let edit req =
  let result ({ Pool_context.database_label; _ } as context) =
    let id = HttpUtils.find_id Tags.Id.of_string Field.Tag req in
    let* tag = Tags.find database_label id >|- Response.not_found in
    Page.Admin.Settings.Tags.edit context tag
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    |> Response.bad_request_render_error context
  in
  Response.handle ~src req result
;;

let write action req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let error_handler, success =
    let open Pool_message.Success in
    match action with
    | `Create -> new_form, Created field
    | `Update _ -> edit, Updated field
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error ~urlencoded error_handler
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Tags_command in
      let is_existing ?exclude_id ({ title; model; _ } as data : decoded) =
        if%lwt Tags.already_exists ?exclude_id database_label title model
        then Lwt.return_error (Pool_message.Error.AlreadyExisting Field.Tag)
        else Lwt.return_ok data
      in
      match action with
      | `Create ->
        Create.(urlencoded |> decode |> Lwt_result.lift >>= is_existing >== handle ~tags)
      | `Update id ->
        let* ({ Tags.id; _ } as tag) = Tags.find database_label id in
        Update.(
          urlencoded
          |> decode
          |> Lwt_result.lift
          >>= is_existing ~exclude_id:id
          >== handle ~tags tag)
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        base_path
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let create = write `Create
let update req = write (`Update (id req)) req
let search = Helpers.Search.htmx_search_helper `ContactTag

let changelog req =
  let id = id req in
  let url = HttpUtils.Url.Admin.Settings.tags_path ~suffix:"changelog" ~id () in
  Helpers.Changelog.htmx_handler ~url (Tags.Id.to_common id) req
;;

module Access : sig
  include module type of Helpers.Access

  val assign_tag_to_contact : Rock.Middleware.t
  val remove_tag_from_contact : Rock.Middleware.t
  val assign_tag_to_experiment : Rock.Middleware.t
  val remove_tag_from_experiment : Rock.Middleware.t
  val search : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian
  module Command = Cqrs_command.Tags_command

  let contact_effects = Guardian.id_effects Contact.Id.validate Field.Contact
  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment
  let tag_effects = Guardian.id_effects Tags.Id.validate Field.Tag
  let index = Tags.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let read = tag_effects Tags.Guard.Access.read
  let update = tag_effects Command.Update.effects
  let assign_tag_to_contact = contact_effects Command.AssignTagToContact.effects
  let remove_tag_from_contact = contact_effects Command.RemoveTagFromContact.effects
  let assign_tag_to_experiment = experiment_effects Command.AssignTagToExperiment.effects

  let remove_tag_from_experiment =
    experiment_effects Command.RemoveTagFromExperiment.effects
  ;;

  let search = Tags.Guard.Access.read_entity |> Guardian.validate_admin_entity
end
