open CCFun
open Utils.Lwt_result.Infix

let find_roles database_label admin : Guard.ActorRole.t list Lwt.t =
  Pool_context.Admin admin
  |> Pool_context.Utils.find_authorizable_opt database_label
  >|> CCOption.map_or ~default:(Lwt.return []) (fun { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor
      ~ctx:(Pool_database.to_ctx database_label)
      uuid)
;;

let find_roles_of_ctx { Pool_context.database_label; user; _ } =
  user
  |> Pool_context.Utils.find_authorizable_opt database_label
  >|> CCOption.map_or ~default:(Lwt.return []) (fun { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor
      ~ctx:(Pool_database.to_ctx database_label)
      uuid)
;;

let has_permission set database_label user =
  let open Utils.Lwt_result.Infix in
  Pool_context.Utils.find_authorizable database_label user
  >>= Guard.Persistence.validate database_label set
  ||> CCResult.is_ok
;;

let can_read_contact_name = has_permission Contact.Guard.Access.read_name
let can_read_contact_info = has_permission Contact.Guard.Access.read_info
let can_access_contact_profile = has_permission Contact.Guard.Access.index

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
