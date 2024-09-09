let map = CCOption.map

let append_opt suffix path =
  suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
;;

module Admin = struct
  let admin_path ?suffix ?id () =
    "/admin/admins" |> append_opt (map Admin.Id.value id) |> append_opt suffix
  ;;

  let filter_path ?suffix ?id () =
    "/admin/filter/" |> append_opt Filter.(map Id.value id) |> append_opt suffix
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

  let experiment_path ?suffix ?id () =
    "/admin/experiments"
    |> append_opt (map Experiment.Id.value id)
    |> append_opt suffix
  ;;

  let invitations_path ?suffix experiment_id =
    let suffix = Format.asprintf "/invitations" |> append_opt suffix in
    experiment_path ~suffix ~id:experiment_id ()
  ;;

  let session_path ?suffix ?id experiment_id =
    let suffix =
      Format.asprintf "/sessions"
      |> append_opt Session.(map Id.value id)
      |> append_opt suffix
    in
    experiment_path ~suffix ~id:experiment_id ()
  ;;

  let location_path ?suffix ?id () =
    "/admin/locations/"
    |> append_opt Pool_location.(map Id.value id)
    |> append_opt suffix
  ;;

  let organisational_unit_path ?suffix ?id () =
    "/admin/organisational-unit"
    |> append_opt (map Organisational_unit.Id.value id)
    |> append_opt suffix
  ;;
end

module Contact = struct
  let experiment_path ?suffix ?id () =
    "/experiments"
    |> append_opt (map Experiment.Id.value id)
    |> append_opt suffix
  ;;
end
