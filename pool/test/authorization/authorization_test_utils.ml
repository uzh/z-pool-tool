let pool = Test_utils.Data.database_label
let to_role (admin, role, target_uuid) = Guard.ActorRole.create ?target_uuid admin role
let admin_target admin = admin |> Admin.id |> Guard.Uuid.target_of Admin.Id.value

let contact_target contact =
  contact |> Contact.id |> Guard.Uuid.target_of Contact.Id.value
;;

let experiment_target { Experiment.id; _ } =
  id |> Guard.Uuid.target_of Experiment.Id.value
;;

let location_target { Pool_location.id; _ } =
  id |> Guard.Uuid.target_of Pool_location.Id.value
;;

let session_target { Session.id; _ } = id |> Guard.Uuid.target_of Session.Id.value

let calendar_target ({ Session.Calendar.id; _ } : Session.Calendar.t) =
  id |> Guard.Uuid.target_of Session.Id.value
;;

let assign_role { Guard.Actor.uuid; _ } role target =
  let open Guard in
  RolesGranted [ (uuid, role, target) |> to_role ] |> handle_event pool
;;

let revoke_role { Guard.Actor.uuid; _ } role target =
  let open Guard in
  RolesRevoked [ (uuid, role, target) |> to_role ] |> handle_event pool
;;

let create_actor_permission { Guard.Actor.uuid; _ } permission target =
  let open Guard in
  ActorPermissionSaved [ ActorPermission.create_for_id uuid permission target ]
  |> handle_event pool
;;

let actor_permissions actor =
  actor.Guard.Actor.uuid |> Guard.Persistence.ActorRole.permissions_of_actor pool
;;
