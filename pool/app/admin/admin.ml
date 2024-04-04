open CCFun.Infix
open Utils.Lwt_result.Infix
include Event
include Entity

let find = Repo.find
let find_by_email = Repo.find_by_email
let find_by = Repo.find_by

let find_all_id_with_role ?exclude pool role =
  Guard.Persistence.ActorRole.find_actors_by_role
    ~ctx:(Database.to_ctx pool)
    ?exclude
    role
  ||> CCList.map CCFun.(Guard.Uuid.Actor.to_string %> Pool_common.Id.of_string)
;;

let find_all_with_role ?exclude pool role =
  find_all_id_with_role ?exclude pool role >|> Repo.find_multiple pool
;;

let find_all_with_roles ?exclude pool roles =
  Lwt_list.map_s (find_all_id_with_role ?exclude pool) roles
  ||> CCList.flatten %> CCList.uniq ~eq:Id.equal
  >|> Repo.find_multiple pool
;;

let search_by_name_and_email = Repo.Sql.search_by_name_and_email

let user_is_admin pool (user : Sihl_user.t) =
  if Sihl_user.is_admin user
  then (
    let%lwt admin = find pool (Pool_common.Id.of_string user.Sihl_user.id) in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;

module Guard = Entity_guard

module Repo = struct
  module Entity = Repo_entity

  let sql_select_columns = Repo.Sql.sql_select_columns
  let joins = Repo.Sql.joins
end
