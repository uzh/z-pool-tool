open CCFun.Infix
open Utils.Lwt_result.Infix
include Event
include Entity

let find = Repo.find
let find_all = Repo.find_all

let find_all_with_role ?exclude pool role =
  let open CCList in
  Guard.Persistence.Actor.find_by_role
    ~ctx:(Pool_database.to_ctx pool)
    ?exclude
    role
  ||> map CCFun.(Guard.Uuid.Actor.to_string %> Pool_common.Id.of_string)
  >|> Repo.find_multiple pool
;;

let find_all_with_roles ?exclude pool roles =
  let open CCList in
  let open Guard in
  Persistence.Actor.find_by_roles
    ~ctx:(Pool_database.to_ctx pool)
    ?exclude
    roles
  ||> fold_left
        (fun acc (_, actors) ->
          acc @ (map (Uuid.Actor.to_string %> Id.of_string)) actors)
        []
  ||> uniq ~eq:Id.equal
  >|> Repo.find_multiple pool
;;

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

  let select_imported_admins_sql = Repo.Sql.select_imported_admins_sql
end
