open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let field = Field.Tag
let base_path = "/admin/settings/tags"
let error_path = "/admin/settings/tags"
let src = Logs.Src.create "handler.admin.settings_tags"
let active_navigation = base_path
let id req = Sihl.Web.Router.param req @@ Field.show field |> Tags.Id.of_string

let index req =
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path
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
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.Settings.Tags.new_form ~flash_fetcher context
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, Format.asprintf "%s/create" base_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let edit req =
  let result ({ Pool_context.database_label; _ } as context) =
    let id = HttpUtils.find_id Tags.Id.of_string Field.Tag req in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Tags.find database_label id
    >|+ Page.Admin.Settings.Tags.edit ~flash_fetcher context
    >>= General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, Format.asprintf "%s/edit" base_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let write action req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect, success =
    let open Pool_message.Success in
    match action with
    | `Create -> Format.asprintf "%s/create" base_path, Created field
    | `Update id -> Format.asprintf "%s/%s" base_path (Tags.Id.value id), Updated field
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect, [ HttpUtils.urlencoded_to_flash urlencoded ])
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
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let create = write `Create
let update req = write (`Update (id req)) req
let search = Helpers.Search.htmx_search_helper `ContactTag

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
