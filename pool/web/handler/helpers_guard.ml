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

let has_permission set database_label user =
  let open Utils.Lwt_result.Infix in
  Pool_context.Utils.find_authorizable database_label user
  >>= Guard.Persistence.validate database_label set
  ||> CCResult.is_ok
;;

let can_view_contact_name = has_permission Contact.Guard.Access.read_name
let can_view_contact_email = has_permission Contact.Guard.Access.read_email

let can_view_contact_cellphone =
  has_permission Contact.Guard.Access.read_cellphone
;;

let can_access_contact_profile = has_permission Contact.Guard.Access.index
