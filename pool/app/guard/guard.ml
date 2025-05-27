include Core
include Event
module Persistence = Repo

(** [console_authorizable] is an [Actor.t] for use in
    administrative tasks, such as working with the command line or running
    tests. *)
let console_authorizable : Actor.t = Actor.create `System (Uuid.Actor.create ())

(** [guest_authorizable] is a [Actor.t] to be assigned to
    entities at the absolute lowest level of trust, such as users browsing the
    public facing website without logging in. *)
let guest_authorizable : Actor.t = Actor.create `Guest (Uuid.Actor.create ())

(** The list of permissions that we need [Guardian] to be aware of in order to
    achieve a minimal level of functionality. Notably, the [`Operator] role should
    have [Manage] authority on everything in the system. *)
let map_role_permission =
  CCList.map (fun (role, permission, model) ->
    RolePermission.create role permission model)
;;

let location_manager_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `LocationManager, Manage, `Location
  ; `LocationManager, Manage, `LocationFile
  ; `LocationManager, Read, `ContactName
  ; `LocationManager, Read, `ContactInfo
  ; `LocationManager, Read, `Session
  ]
  |> map_role_permission
;;

let location_manager_role_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `LocationManager, Read, `RoleLocationManager
  ; `LocationManager, Create, `RoleLocationManager
  ; `LocationManager, Read, `RoleAssistant
  ; `LocationManager, Read, `RoleExperimenter
  ; `LocationManager, Read, `RoleRecruiter
  ]
  |> map_role_permission
;;

let recruiter_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Recruiter, Create, `Admin
  ; `Recruiter, Read, `Admin
  ; `Recruiter, Update, `Admin
  ; `Recruiter, Manage, `Assignment
  ; `Recruiter, Manage, `Contact
  ; `Recruiter, Manage, `CustomField
  ; `Recruiter, Manage, `CustomFieldGroup
  ; `Recruiter, Manage, `Experiment
  ; `Recruiter, Manage, `Filter
  ; `Recruiter, Manage, `I18n
  ; `Recruiter, Manage, `Invitation
  ; `Recruiter, Manage, `Location
  ; `Recruiter, Manage, `LocationFile
  ; `Recruiter, Manage, `Mailing
  ; `Recruiter, Manage, `MessageTemplate
  ; `Recruiter, Read, `Permission
  ; `Recruiter, Read, `OrganisationalUnit
  ; `Recruiter, Read, `Queue
  ; `Recruiter, Manage, `Role
  ; `Recruiter, Read, `Schedule
  ; `Recruiter, Manage, `Session
  ; `Recruiter, Read, `SystemSetting
  ; `Recruiter, Read, `Smtp
  ; `Recruiter, Read, `Statistics
  ; `Recruiter, Read, `System
  ; `Recruiter, Manage, `Tag
  ; `Recruiter, Read, `Tenant
  ; `Recruiter, Manage, `WaitingList
  ]
  |> map_role_permission
;;

let recruiter_role_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Recruiter, Manage, `ContactDirectMessage
  ; `Recruiter, Manage, `InvitationNotification
  ; `Recruiter, Manage, `RoleAdmin
  ; `Recruiter, Manage, `RoleAssistant
  ; `Recruiter, Manage, `RoleExperimenter
  ; `Recruiter, Manage, `RoleLocationManager
  ; `Recruiter, Read, `RoleRecruiter
  ; `Recruiter, Create, `RoleRecruiter
  ; `Recruiter, Read, `RoleOperator
  ]
  |> map_role_permission
;;

let assistant_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Assistant, Read, `Contact
  ; `Assistant, Update, `Contact
  ; `Assistant, Create, `Message
  ; `Assistant, Read, `Session
  ; `Assistant, Read, `SessionClose
  ; `Assistant, Update, `SessionClose
  ; `Assistant, Read, `Assignment
  ; `Assistant, Update, `Assignment
  ; `Assistant, Read, `Experiment
  ; `Assistant, Read, `WaitingList
  ; `Assistant, Update, `WaitingList
  ]
  |> map_role_permission
;;

let assistant_role_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Assistant, Read, `RoleAdmin
  ; `Assistant, Create, `RoleAdmin
  ; `Assistant, Read, `RoleAssistant
  ; `Assistant, Create, `RoleAssistant
  ; `Assistant, Read, `RoleExperimenter
  ; `Assistant, Read, `RoleLocationManager
  ; `Assistant, Read, `RoleRecruiter
  ]
  |> map_role_permission
;;

let experimenter_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Experimenter, Read, `ContactName
  ; `Experimenter, Read, `Session
  ; `Experimenter, Read, `SessionClose
  ; `Experimenter, Update, `SessionClose
  ; `Experimenter, Read, `Assignment
  ; `Experimenter, Update, `Assignment
  ; `Experimenter, Read, `Experiment
  ; `Experimenter, Read, `RoleExperimenter
  ]
  |> map_role_permission
;;

let experimenter_role_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Experimenter, Read, `RoleAdmin
  ; `Experimenter, Read, `RoleAssistant
  ; `Experimenter, Read, `RoleExperimenter
  ; `Experimenter, Read, `RoleLocationManager
  ; `Experimenter, Read, `RoleRecruiter
  ]
  |> map_role_permission
;;

let operator_permissions role_assignments : RolePermission.t list =
  let open Core.Permission in
  Role.Target.all
  |> CCList.filter (fun m ->
    CCList.exists
      (Role.Target.equal m)
      (Role.Role.all |> CCList.map Core.Utils.find_assignable_target_role)
    == role_assignments)
  |> CCList.flat_map (fun role -> [ `Operator, Manage, role ])
  |> map_role_permission
;;

let all_role_permissions =
  operator_permissions false
  @ recruiter_permissions
  @ assistant_permissions
  @ experimenter_permissions
  @ location_manager_permissions
;;

let all_role_assignment_permissions =
  operator_permissions true
  @ recruiter_role_permissions
  @ assistant_role_permissions
  @ experimenter_role_permissions
  @ location_manager_role_permissions
;;

let actor_to_model_permission pool permission model actor =
  let open Utils.Lwt_result.Infix in
  Persistence.ActorRole.permissions_of_actor pool actor.Actor.uuid
  ||> PermissionOnTarget.permission_of_model permission model
;;

let sql_where_fragment ?(field = "uuid") pool permission model actor =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  actor_to_model_permission pool permission model actor
  ||> function
  | true, _ -> None
  | false, [] -> Some "FALSE"
  | false, ids ->
    ids
    |> CCList.map (Uuid.Target.to_string %> Pool_common.Id.sql_value_fragment)
    |> CCString.concat ", "
    |> Format.asprintf {sql| %s IN (%s) |sql} field
    |> CCOption.return
;;

let sql_uuid_list_fragment pool permission model actor =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  actor_to_model_permission pool permission model actor
  ||> function
  | true, _ -> None
  | false, [] -> Some "(NULL)"
  | false, ids ->
    ids
    |> CCList.map
         (Uuid.Target.to_string
          %> Format.asprintf "'%s'"
          %> Pool_common.Id.sql_value_fragment)
    |> CCString.concat ", "
    |> Format.asprintf "(%s)"
    |> CCOption.return
;;

let create_where
      ?actor
      ?permission
      ?(checks : (string -> string) list = [])
      ?all
      pool
      model
  =
  let open Utils.Lwt_result.Infix in
  let open Pool_message in
  let tags = Database.Logger.Tags.create pool in
  let log_warning = Pool_common.Utils.with_log_error ~src ~level:Logs.Warning ~tags in
  match actor, permission with
  | Some actor, Some permission ->
    sql_uuid_list_fragment pool permission model actor
    ||> (function
     | Some uuid_list ->
       CCList.map (fun fcn -> fcn uuid_list) checks
       |> CCString.concat " OR "
       |> Format.asprintf "(%s)"
       |> CCOption.return
     | None -> all)
  | None, Some _ ->
    let (_ : Error.t) = log_warning (Error.Undefined Field.Actor) in
    Lwt.return_some "FALSE"
  | Some _, None ->
    let (_ : Error.t) = log_warning (Error.Undefined Field.Permission) in
    Lwt.return_some "FALSE"
  | None, None -> Lwt.return_none
;;

module Access = struct
  open ValidationSet
  open Permission

  module Role = struct
    let create = one_of_tuple (Create, `Role, None)
    let read = one_of_tuple (Read, `Role, None)
    let update = one_of_tuple (Update, `Role, None)
    let delete = one_of_tuple (Delete, `Role, None)
    let manage = one_of_tuple (Manage, `Role, None)

    module Assignment = struct
      module Assistant = struct
        let model = `RoleAssistant

        let create ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Create model |> one
        ;;

        let read ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Read model |> one
        ;;

        let delete ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Delete model |> one
        ;;

        let manage ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Manage model |> one
        ;;
      end

      module Experimenter = struct
        let model = `RoleExperimenter

        let create ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Create model |> one
        ;;

        let read ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Read model |> one
        ;;

        let delete ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Delete model |> one
        ;;

        let manage ?target_uuid () =
          PermissionOnTarget.create ?target_uuid Manage model |> one
        ;;
      end
    end
  end

  module Permission = struct
    let create = one_of_tuple (Create, `Permission, None)
    let read = one_of_tuple (Read, `Permission, None)
    let update = one_of_tuple (Update, `Permission, None)
    let delete = one_of_tuple (Delete, `Permission, None)
    let manage = one_of_tuple (Manage, `Permission, None)
  end
end

module RolePermission = struct
  include RolePermission
  open Pool_message

  let column_role = (Field.Role, "role_permissions.role") |> Query.Column.create

  let column_target =
    (Field.Target, "role_permissions.target_model") |> Query.Column.create
  ;;

  let column_action = (Field.Action, "role_permissions.permission") |> Query.Column.create

  let column_created_at =
    (Field.CreatedAt, "role_permissions.created_at") |> Query.Column.create
  ;;

  let filterable_by = None
  let searchable_by = [ column_role; column_target; column_action ]
  let sortable_by = column_created_at :: searchable_by

  let default_sort =
    Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
  ;;

  let default_query = Query.create ~sort:default_sort ()
end

module ActorPermission = struct
  include ActorPermission
  open Pool_message

  let column_actor = (Field.Actor, "actor_permissions.actor_uuid") |> Query.Column.create

  let column_action =
    (Field.Permission, "actor_permissions.permission") |> Query.Column.create
  ;;

  let column_model =
    (Field.Model, "actor_permissions.target_model") |> Query.Column.create
  ;;

  let column_target =
    (Field.Target, "actor_permissions.target_uuid") |> Query.Column.create
  ;;

  let column_created_at =
    (Field.CreatedAt, "actor_permissions.created_at") |> Query.Column.create
  ;;

  let filterable_by = None
  let searchable_by = [ column_actor; column_action; column_model; column_target ]
  let sortable_by = column_created_at :: searchable_by

  let default_sort =
    Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
  ;;

  let default_query = Query.create ~sort:default_sort ()
end
