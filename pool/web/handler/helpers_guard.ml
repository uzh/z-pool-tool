open CCFun
open Utils.Lwt_result.Infix

let find_roles database_label admin =
  Pool_context.Admin admin
  |> Pool_context.Utils.find_authorizable_opt database_label
  ||> CCOption.map_or ~default:[] Guard.(Actor.roles %> RoleSet.to_list)
;;

let find_roles_by_user database_label user =
  Pool_context.Utils.find_authorizable_opt database_label user
  ||> CCOption.map_or
        ~default:[]
        (Guard.Persistence.Actor.expand_roles %> Guard.RoleSet.to_list)
;;

let find_roles_of_ctx { Pool_context.database_label; user; _ } =
  find_roles_by_user database_label user
;;
