let remove_admin_guardian_role_permission =
  Database.Migration.Step.create
    ~label:"remove admin guardian_role_permission"
    [%string
      {sql|
        DELETE FROM guardian_role_permissions
        WHERE role = "`Admin";
      |sql}]
;;

let remove_admin_guardian_assign_roles =
  Database.Migration.Step.create
    ~label:"remove admin guardian_assign_roles"
    [%string
      {sql|
        DELETE FROM guardian_assign_roles
        WHERE role = "`Admin";
      |sql}]
;;

let remove_admin_guardian_actor_roles =
  Database.Migration.Step.create
    ~label:"remove admin guardian_actor_roles"
    [%string
      {sql|
        DELETE FROM guardian_actor_roles
        WHERE role = "`Admin";
      |sql}]
;;

let remove_admin_guardian_actor_role_targets =
  Database.Migration.Step.create
    ~label:"remove admin guardian_actor_role_targets"
    [%string
      {sql|
        DELETE FROM guardian_actor_role_targets
        WHERE role = "`Admin";
      |sql}]
;;

let migration () =
  Database.Migration.(
    empty "202503251234"
    |> add_step remove_admin_guardian_role_permission
    |> add_step remove_admin_guardian_assign_roles
    |> add_step remove_admin_guardian_actor_roles
    |> add_step remove_admin_guardian_actor_role_targets)
;;
