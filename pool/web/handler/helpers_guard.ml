open Utils.Lwt_result.Infix

let find_roles database_label admin =
  Pool_context.Admin admin
  |> Pool_context.Utils.find_authorizable_opt database_label
  >|> CCOption.map_or ~default:(Lwt.return []) (fun { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor database_label uuid)
;;

let find_roles_of_ctx { Pool_context.database_label; user; _ } =
  user
  |> Pool_context.Utils.find_authorizable_opt database_label
  >|> CCOption.map_or ~default:(Lwt.return []) (fun { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor database_label uuid)
;;

let has_permission database_label user set =
  let open Utils.Lwt_result.Infix in
  Pool_context.Utils.find_authorizable database_label user
  >>= Guard.Persistence.validate database_label set
  ||> CCResult.is_ok
;;

let can_read_contact permisson_on_target { Pool_context.guardian; _ } =
  let open Guard in
  let open PermissionOnTarget in
  permisson_on_target
  |> CCList.fold_left
       (fun init set -> if init then init else validate set guardian)
       false
;;

let can_read_contact_name context verify_on_ids =
  can_read_contact Contact.Guard.Access.(read_name ~verify_on_ids ()) context
;;

let can_read_contact_info context verify_on_ids =
  can_read_contact Contact.Guard.Access.(read_info ~verify_on_ids ()) context
;;

let can_access_contact_profile context id =
  can_read_contact
    Contact.Guard.Access.(
      read_of_target (Guard.Uuid.target_of Experiment.Id.value id))
    context
;;

let target_model_for_actor_role
  pool
  ({ Guard.ActorRole.target_uuid; _ } as role)
  =
  let find_target_model =
    CCOption.map_or ~default:Lwt.return_none (fun uuid ->
      Guard.Persistence.Target.find_model ~ctx:(Pool_database.to_ctx pool) uuid
      ||> CCResult.to_opt)
  in
  let%lwt target_model = find_target_model target_uuid in
  Lwt.return (role, target_model)
;;

let can_send_direct_message { Pool_context.database_label; user; _ } =
  has_permission database_label user Contact.Guard.Access.send_direct_message
;;

let can_rerun_session_filter
  { Pool_context.database_label; user; _ }
  experiment_id
  session_id
  =
  has_permission
    database_label
    user
    (Cqrs_command.Assignment_command.UpdateMatchesFilter.effects
       experiment_id
       session_id)
;;
