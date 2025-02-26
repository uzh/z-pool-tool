module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.experiments_users"
let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_message.Field.Experiment
;;

let session_id = HttpUtils.find_id Session.Id.of_string Pool_message.Field.Session
let admin_id = HttpUtils.find_id Admin.Id.of_string Pool_message.Field.Admin

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

let index entity role req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; language; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let id = experiment_id req in
    let form_path, guard_id = entity_path_and_guard id req role entity in
    let current_roles, exclude =
      match role with
      | `Assistants -> (`Assistant, Some guard_id), [ `Assistant, None ]
      | `Experimenter -> (`Experimenter, Some guard_id), [ `Experimenter, None ]
    in
    let query =
      let open Admin in
      Query.from_request
        ?filterable_by
        ~searchable_by
        ~sortable_by
        ~default:default_query
        req
    in
    let%lwt applicable_admins =
      Admin.(
        query_by_role
          ~query
          database_label
          (`Admin, None)
          ~exclude:(current_roles :: exclude))
    in
    let%lwt currently_assigned =
      Admin.(query_by_role ~query database_label current_roles ~exclude)
    in
    let%lwt hint =
      (match role with
       | `Assistants -> I18n.Key.AssistantRoleHint
       | `Experimenter -> I18n.Key.ExperimenterRoleHint)
      |> CCFun.flip (I18n.find_by_key database_label) language
    in
    let* experiment = Experiment.find database_label id in
    let%lwt can_assign, can_unassign =
      match%lwt Pool_context.Utils.find_authorizable_opt database_label user with
      | None -> Lwt.return (false, false)
      | Some actor ->
        let open Guard in
        let role, target_uuid = current_roles in
        let check permission =
          PermissionOnTarget.create
            ?target_uuid
            permission
            (role |> Utils.find_assignable_target_role)
          |> ValidationSet.one
          |> CCFun.flip (Guard.Persistence.validate database_label) actor
          ||> CCResult.is_ok
        in
        Lwt.both (check Permission.Create) (check Permission.Delete)
    in
    Page.Admin.Experiments.users
      ~hint
      ~can_assign
      ~can_unassign
      entity
      role
      experiment
      form_path
      applicable_admins
      currently_assigned
      context
    |> Lwt_result.ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let index_assistants = index `Experiment `Assistants
let index_experimenter = index `Experiment `Experimenter
let index_session_assistants = index `Session `Assistants

let query_admin entity role state req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let id = experiment_id req in
    let form_path, guard_id = entity_path_and_guard id req role entity in
    let current_roles : Role.Role.t * Guard.Uuid.Target.t option =
      match role with
      | `Assistants -> `Assistant, Some guard_id
      | `Experimenter -> `Experimenter, Some guard_id
    in
    let%lwt admins =
      let open Admin in
      let query =
        Query.from_request
          ?filterable_by
          ~searchable_by
          ~sortable_by
          ~default:default_query
          req
      in
      match state with
      | `Assigned -> query_by_role database_label ~query ?exclude:None current_roles
      | `Available ->
        query_by_role database_label ~query ~exclude:[ current_roles ] (`Admin, None)
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
      | Some actor ->
        let open Guard in
        let role, target_uuid = current_roles in
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
    in
    let open Page.Admin.Experiments.User in
    (match state with
     | `Assigned -> list_existing context form_path ~can_unassign:permission admins
     | `Available -> list_available context form_path ~can_assign:permission admins)
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
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
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let* admin = Admin.find database_label admin_id in
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
  result |> HttpUtils.extract_happy_path ~src req
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
