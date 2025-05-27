module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.experiments_users"
let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_message.Field.Experiment
;;

let session_id = HttpUtils.find_id Session.Id.of_string Pool_message.Field.Session
let admin_id = HttpUtils.find_id Admin.Id.of_string Pool_message.Field.Admin

let has_permission_on_role database_label actor role permission =
  let open Utils.Lwt_result.Infix in
  let open Guard in
  let role, target_uuid = role in
  let check permission =
    PermissionOnTarget.create
      ?target_uuid
      permission
      (role |> Utils.find_assignable_target_role)
    |> ValidationSet.one
    |> CCFun.flip (Guard.Persistence.validate database_label) actor
    ||> CCResult.is_ok
  in
  check permission
;;

let field_of_role =
  let open Pool_message in
  function
  | `Assistants -> Field.Assistants
  | `Experimenter -> Field.Experimenter
;;

let entity_path_and_guard experiment_id req role =
  let open HttpUtils.Url.Admin in
  let field = field_of_role role in
  function
  | `Experiment ->
    let path = experiment_user_path experiment_id field in
    let id = experiment_id |> Guard.Uuid.target_of Experiment.Id.value in
    path, id
  | `Session ->
    let session_id = session_id req in
    let path = session_user_path experiment_id session_id field in
    let id = session_id |> Guard.Uuid.target_of Session.Id.value in
    path, id
;;

let target_has_role db admin target_role () =
  let open Utils.Lwt_result.Infix in
  let open Guard in
  let actor = Uuid.actor_of Admin.Id.value (Admin.id admin) in
  let actor_role = ActorRole.create actor target_role in
  let%lwt actor_roles =
    Persistence.ActorRole.find_by_actor db actor ||> CCList.map (fun (role, _, _) -> role)
  in
  actor_roles |> CCList.mem ~eq:ActorRole.equal actor_role |> Lwt.return
;;

let query_by_role database_label query global_role ?exclude role =
  let open Utils.Lwt_result.Infix in
  Admin.query_by_role ~query database_label role ?exclude
  >|> fun (admins, query) ->
  let%lwt admins =
    Lwt_list.map_s
      (fun a -> target_has_role database_label a global_role () ||> CCPair.make a)
      admins
  in
  Lwt.return (admins, query)
;;

let query_admin_current_and_exclude_role role guard_id =
  match role with
  | `Assistants -> (`Assistant, Some guard_id), `Assistant
  | `Experimenter -> (`Experimenter, Some guard_id), `Experimenter
;;

let index entity role req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; language; user; _ } as context) =
    let id = experiment_id req in
    let* experiment = Experiment.find database_label id >|- Response.not_found in
    Response.bad_request_render_error context
    @@
    let form_path, guard_id = entity_path_and_guard id req role entity in
    let current_roles, global_role = query_admin_current_and_exclude_role role guard_id in
    let query = Admin.query_from_request req in
    let query_by_role ?exclude role =
      query_by_role database_label query global_role ?exclude role
    in
    let%lwt applicable_admins =
      query_by_role None ~exclude:[ current_roles; global_role, None ]
    in
    let%lwt currently_assigned =
      query_by_role (Some [ current_roles; global_role, None ])
    in
    let%lwt hint =
      (match role with
       | `Assistants -> I18n.Key.AssistantRoleHint
       | `Experimenter -> I18n.Key.ExperimenterRoleHint)
      |> CCFun.flip (I18n.find_by_key database_label) language
    in
    let%lwt can_assign, can_unassign =
      match%lwt Pool_context.Utils.find_authorizable_opt database_label user with
      | None -> Lwt.return (false, false)
      | Some actor ->
        let open Guard in
        let check = has_permission_on_role database_label actor current_roles in
        Lwt.both (check Permission.Create) (check Permission.Delete)
    in
    Page.Admin.Experiments.users
      ~hint
      ~can_assign
      ~can_unassign
      entity
      global_role
      experiment
      form_path
      applicable_admins
      currently_assigned
      context
    |> Lwt_result.ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let index_assistants = index `Experiment `Assistants
let index_experimenter = index `Experiment `Experimenter
let index_session_assistants = index `Session `Assistants

let query_admin entity role state req =
  let result ({ Pool_context.database_label; user; _ } as context) =
    let id = experiment_id req in
    let form_path, guard_id = entity_path_and_guard id req role entity in
    let current_roles, global_role = query_admin_current_and_exclude_role role guard_id in
    let%lwt admins =
      let query = Admin.query_from_request req in
      let query_by_role ?exclude role =
        query_by_role database_label query global_role ?exclude role
      in
      match state with
      | `Assigned -> query_by_role (Some [ current_roles; global_role, None ])
      | `Available -> query_by_role ~exclude:[ current_roles; global_role, None ] None
    in
    let%lwt permission =
      let open Guard in
      let permission =
        match state with
        | `Assigned -> Permission.Delete
        | `Available -> Permission.Create
      in
      match%lwt Pool_context.Utils.find_authorizable_opt database_label user with
      | None -> Lwt.return false
      | Some actor -> has_permission_on_role database_label actor current_roles permission
    in
    let open Page.Admin.Experiments.User in
    (match state with
     | `Assigned ->
       list_existing context form_path ~can_unassign:permission ~role:global_role admins
     | `Available ->
       list_available context form_path ~can_assign:permission ~role:global_role admins)
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  result |> Response.Htmx.handle ~src req
;;

module Assistant = struct
  let assigned = query_admin `Experiment `Assistants `Assigned
  let available = query_admin `Experiment `Assistants `Available
end

module Experimenter = struct
  let assigned = query_admin `Experiment `Experimenter `Assigned
  let available = query_admin `Experiment `Experimenter `Available
end

module SessionAssistant = struct
  let assigned = query_admin `Session `Assistants `Assigned
  let available = query_admin `Session `Assistants `Available
end

let toggle_role entity action req =
  let experiment_id = experiment_id req in
  let admin_id = admin_id req in
  let base_path =
    let open HttpUtils.Url.Admin in
    match entity with
    | `Experiment -> fun suffix -> experiment_path ~id:experiment_id ~suffix ()
    | `Session -> fun suffix -> session_path experiment_id ~id:(session_id req) ~suffix
  in
  let redirect_path =
    (match action with
     | `AssignAssistant | `UnassignAssistant -> "assistants"
     | `AssignExperimenter | `UnassignExperimenter -> "experimenter")
    |> base_path
  in
  let fallback_handler =
    match action with
    | `AssignAssistant | `UnassignAssistant ->
      (match entity with
       | `Experiment -> index_assistants
       | `Session -> index_session_assistants)
    | `AssignExperimenter | `UnassignExperimenter -> index_experimenter
  in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    let* experiment =
      Experiment.find database_label experiment_id >|- Response.not_found
    in
    let* admin = Admin.find database_label admin_id >|- Response.not_found in
    Response.bad_request_on_error fallback_handler
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let message =
      let open Pool_message.Success in
      match action with
      | `AssignAssistant | `AssignExperimenter -> RoleAssigned
      | `UnassignAssistant | `UnassignExperimenter -> RoleUnassigned
    in
    let* events =
      match entity with
      | `Experiment ->
        let open Cqrs_command.Experiment_command in
        let update = { admin; experiment } in
        Lwt_result.lift
        @@
          (match action with
          | `AssignAssistant -> AssignAssistant.(handle ~tags update)
          | `UnassignAssistant -> UnassignAssistant.(handle ~tags update)
          | `AssignExperimenter -> AssignExperimenter.(handle ~tags update)
          | `UnassignExperimenter -> UnassignExperimenter.(handle ~tags update))
      | `Session ->
        let open Cqrs_command.Session_command in
        let* session = Session.find database_label (session_id req) in
        let update = { admin; session } in
        Lwt_result.lift
        @@
          (match action with
          | `AssignAssistant -> AssignAssistant.(handle ~tags update)
          | `UnassignAssistant -> UnassignAssistant.(handle ~tags update)
          | `AssignExperimenter | `UnassignExperimenter ->
            failwith "Experimenter does not exist on session level")
    in
    let%lwt () = Pool_event.handle_events database_label user events in
    Http_utils.redirect_to_with_actions redirect_path [ Message.set ~success:[ message ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let assign_assistant = toggle_role `Experiment `AssignAssistant
let unassign_assistant = toggle_role `Experiment `UnassignAssistant
let assign_experimenter = toggle_role `Experiment `AssignExperimenter
let unassign_experimenter = toggle_role `Experiment `UnassignExperimenter
let assign_session_assistant = toggle_role `Session `AssignAssistant
let unassign_session_assistant = toggle_role `Session `UnassignAssistant

module Access : sig
  include module type of Helpers.Access

  val index_assistants : Rock.Middleware.t
  val assign_assistant : Rock.Middleware.t
  val unassign_assistant : Rock.Middleware.t
  val index_experimenter : Rock.Middleware.t
  val assign_experimenter : Rock.Middleware.t
  val unassign_experimenter : Rock.Middleware.t
end = struct
  include Helpers.Access
  open Guard
  open Cqrs_command.Experiment_command
  module Field = Pool_message.Field

  let experiment_effects =
    Middleware.Guardian.id_effects Experiment.Id.validate Field.Experiment
  ;;

  let index_effects
        (validation_set : ?target_uuid:Uuid.Target.t -> unit -> ValidationSet.t)
    =
    Middleware.Guardian.validate_generic
    @@ fun req ->
    req
    |> HttpUtils.find_id Uuid.Target.of_string Field.Experiment
    |> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
    |> CCResult.map (fun target_uuid -> validation_set ~target_uuid ())
  ;;

  let index_assistants = index_effects Access.Role.Assignment.Assistant.read
  let assign_assistant = experiment_effects AssignAssistant.effects
  let unassign_assistant = experiment_effects UnassignAssistant.effects
  let index_experimenter = index_effects Access.Role.Assignment.Experimenter.read
  let assign_experimenter = experiment_effects AssignExperimenter.effects
  let unassign_experimenter = experiment_effects UnassignExperimenter.effects
end
