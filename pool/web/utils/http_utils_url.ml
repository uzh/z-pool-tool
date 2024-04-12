module Admin = struct
  let map = CCOption.map

  let append_opt suffix path =
    suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
  ;;

  let role_permission_path ?suffix ?role () =
    "/admin/settings/role-permission"
    |> append_opt (map Role.Role.name role)
    |> append_opt suffix
  ;;

  let role_permission_target_path ?suffix ?target role () =
    role_permission_path ~role ()
    |> Format.asprintf "%s/target"
    |> append_opt (map Role.Target.name target)
    |> append_opt suffix
  ;;

  let session_path ?suffix ?id experiment_id =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      Experiment.(Id.value experiment_id)
    |> append_opt Session.(map Id.value id)
    |> append_opt suffix
  ;;
end
