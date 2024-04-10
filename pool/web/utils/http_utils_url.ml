module Admin = struct
  let append_opt suffix path =
    suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
  ;;

  let role_permission_path ?suffix ?role () =
    "/admin/settings/role-permission"
    |> append_opt (CCOption.map Role.Role.name role)
    |> append_opt suffix
  ;;

  let role_permission_target_path ?suffix ?target role () =
    role_permission_path ~role ()
    |> Format.asprintf "%s/target"
    |> append_opt (CCOption.map Role.Target.name target)
    |> append_opt suffix
  ;;

  let session_path ?suffix experiment_id session_id =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      Experiment.(Id.value experiment_id)
      Session.(session_id |> Id.value)
    |> append_opt suffix
  ;;
end
