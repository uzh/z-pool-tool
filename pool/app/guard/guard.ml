include Core
include Event
module Persistence = Repo

(** [console_authorizable] is an Actor.t] for use in
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

let recruiter_permissions : RolePermission.t list =
  let open Core.Permission in
  [ `Recruiter, Read, `Admin
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
  ]
  |> map_role_permission
;;

let operator_permissions : RolePermission.t list =
  let open Core.Permission in
  CCList.flat_map (fun role -> [ `Operator, Manage, role ]) Role.Target.all
  |> map_role_permission
;;

let all_role_permissions =
  operator_permissions
  @ recruiter_permissions
  @ assistant_permissions
  @ experimenter_permissions
  @ location_manager_permissions
;;

let actor_to_model_permission pool permission model actor =
  let open Utils.Lwt_result.Infix in
  Persistence.ActorRole.permissions_of_actor
    ~ctx:(Pool_database.to_ctx pool)
    actor.Actor.uuid
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
    |> CCList.map
         (Uuid.Target.to_string %> Format.asprintf "guardianEncodeUuid('%s')")
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
         (Uuid.Target.to_string %> Format.asprintf "guardianEncodeUuid('%s')")
    |> CCString.concat ", "
    |> Format.asprintf "(%s)"
    |> CCOption.return
;;

let create_where
  ?actor
  ?permission
  ?(checks : (string -> string) list = [])
  pool
  model
  =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_database.Logger.Tags.create pool in
  let log_warning =
    Pool_common.Utils.with_log_error ~src ~level:Logs.Warning ~tags
  in
  match actor, permission with
  | Some actor, Some permission ->
    sql_uuid_list_fragment pool permission model actor
    ||> CCOption.map (fun uuid_list ->
      CCList.map (fun fcn -> fcn uuid_list) checks
      |> CCString.concat " OR "
      |> Format.asprintf "(%s)")
  | None, Some _ ->
    let _ = log_warning Pool_common.Message.(Undefined Field.Actor) in
    Lwt.return_some "FALSE"
  | Some _, None ->
    let _ = log_warning Pool_common.Message.(Undefined Field.Permission) in
    Lwt.return_some "FALSE"
  | None, None -> Lwt.return_none
;;

module Access = struct
  open ValidationSet
  open Permission

  let create_role = one_of_tuple (Create, `Role, None)
  let read_role = one_of_tuple (Read, `Role, None)
  let update_role = one_of_tuple (Update, `Role, None)
  let delete_role = one_of_tuple (Delete, `Role, None)
  let manage_role = one_of_tuple (Manage, `Role, None)
  let manage_permission = one_of_tuple (Manage, `Permission, None)
end
