open CCFun.Infix
open Utils.Lwt_result.Infix
include Event
include Entity
include Repo

let find_all_id_with_role ?exclude pool role =
  Guard.Persistence.ActorRole.find_actors_by_role
    ~ctx:(Database.to_ctx pool)
    ?exclude
    role
  ||> CCList.map CCFun.(Guard.Uuid.Actor.to_string %> Pool_user.Id.of_string)
;;

let find_all_with_role ?exclude pool role =
  find_all_id_with_role ?exclude pool role >|> Repo.find_multiple pool
;;

let find_all_with_roles ?exclude pool roles =
  Lwt_list.map_s (find_all_id_with_role ?exclude pool) roles
  ||> CCList.flatten %> CCList.uniq ~eq:Pool_user.Id.equal
  >|> Repo.find_multiple pool
;;

let find_all_with_permissions_on_target
      database_label
      target
      entity_uuid
      permissions
  =
  let open Utils.Lwt_result.Infix in
  let open Guard in
  Persistence.RolePermission.find_actors_by_target_and_permissions
    database_label
    target
    entity_uuid
    permissions
  ||> CCList.map Pool_user.Id.of_common
  >|> Repo.find_multiple database_label
;;

let user_is_admin pool (user : Pool_user.t) =
  if Pool_user.is_admin user
  then (
    let%lwt admin = user.Pool_user.id |> Id.of_user |> find pool in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;

module Guard = Entity_guard

module Repo = struct
  include Repo_entity

  let sql_select_columns = Repo.sql_select_columns
  let joins = Repo.joins
end
