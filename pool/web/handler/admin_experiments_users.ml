module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.experiments_users"
let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_common.Message.Field.Experiment
;;

let admin_id =
  HttpUtils.find_id Admin.Id.of_string Pool_common.Message.Field.Admin
;;

let index role req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let id = experiment_id req in
    let current_roles =
      let id = id |> Guard.Uuid.target_of Experiment.Id.value in
      match role with
      | `Assistants -> `Assistant, Some id
      | `Experimenter -> `Experimenter, Some id
    in
    let%lwt applicable_admins =
      Admin.find_all_with_role
        database_label
        (`Admin, None)
        ~exclude:[ current_roles ]
    in
    let%lwt currently_assigned =
      Admin.find_all_with_role database_label current_roles
    in
    let* experiment = Experiment.find database_label id in
    Page.Admin.Experiments.users
      role
      experiment
      applicable_admins
      currently_assigned
      context
    |> Lwt_result.ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let index_assistants = index `Assistants
let index_experimenter = index `Experimenter

let toggle_role action req =
  let experiment_id = experiment_id req in
  let admin_id = admin_id req in
  let redirect_path =
    let base_path =
      Format.asprintf
        "/admin/experiments/%s/%s"
        (Experiment.Id.value experiment_id)
    in
    (match action with
     | `AssignAssistant | `UnassignAssistant -> "assistants"
     | `AssignExperimenter | `UnassignExperimenter -> "experimenter")
    |> base_path
  in
  let result { Pool_context.database_label; _ } =
    let open Utils.Lwt_result.Infix in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let* admin = Admin.find database_label admin_id in
    let message =
      let open Pool_common.Message in
      match action with
      | `AssignAssistant | `AssignExperimenter -> RoleAssigned
      | `UnassignAssistant | `UnassignExperimenter -> RoleUnassigned
    in
    let* events =
      let open Cqrs_command.Experiment_command in
      let update = { admin; experiment } in
      Lwt_result.lift
      @@
      match action with
      | `AssignAssistant -> AssignAssistant.(handle ~tags update)
      | `UnassignAssistant -> UnassignAssistant.(handle ~tags update)
      | `AssignExperimenter -> AssignExperimenter.(handle ~tags update)
      | `UnassignExperimenter -> UnassignExperimenter.(handle ~tags update)
    in
    let%lwt () = Pool_event.handle_events database_label events in
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ message ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let assign_assistant = toggle_role `AssignAssistant
let unassign_assistant = toggle_role `UnassignAssistant
let assign_experimenter = toggle_role `AssignExperimenter
let unassign_experimenter = toggle_role `UnassignExperimenter

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
  open ValidationSet
  open Cqrs_command.Experiment_command
  module Field = Pool_common.Message.Field

  let experiment_effects =
    Middleware.Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let index_assistants =
    (fun req ->
      let expand = CCFun.flip experiment_effects req in
      Or [ expand AssignAssistant.effects; expand UnassignAssistant.effects ])
    |> Middleware.Guardian.validate_generic
  ;;

  let assign_assistant =
    AssignAssistant.effects
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let unassign_assistant =
    UnassignAssistant.effects
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let index_experimenter =
    (fun req ->
      let expand = CCFun.flip experiment_effects req in
      Or
        [ expand AssignExperimenter.effects
        ; expand UnassignExperimenter.effects
        ])
    |> Middleware.Guardian.validate_generic
  ;;

  let assign_experimenter =
    AssignExperimenter.effects
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let unassign_experimenter =
    UnassignExperimenter.effects
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
