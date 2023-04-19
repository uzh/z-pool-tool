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

module Filter = struct
  let experiments { Pool_context.database_label; user; _ } experiments =
    let%lwt actor =
      Pool_context.Utils.find_authorizable_opt
        ~admin_only:true
        database_label
        user
    in
    Lwt_list.filter_s
      (fun { Experiment.id; _ } ->
        CCOption.map_or
          ~default:Lwt.return_false
          (fun actor ->
            Guard.Persistence.validate
              database_label
              (Experiment.Guard.Access.read id)
              actor
            ||> CCOption.(of_result %> is_some))
          actor)
      experiments
  ;;
end
