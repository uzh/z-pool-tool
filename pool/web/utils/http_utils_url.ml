open Pool_message.Field

let map = CCOption.map

let append_opt suffix path =
  suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
;;

module Admin = struct
  let settings_path = Format.asprintf "/admin/settings/%s"

  let api_key_path ?suffix ?id () =
    human_url ApiKey
    |> settings_path
    |> append_opt (map Api_key.Id.value id)
    |> append_opt suffix
  ;;

  let role_permission_path ?suffix ?role () =
    settings_path "role-permission"
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

  module Settings = struct
    let queue_list_path ?suffix table =
      let table =
        match table with
        | `Current -> ""
        | `History -> "/archive"
      in
      Format.asprintf "/admin/settings/queue%s" table |> append_opt suffix
    ;;

    let queue_path ?suffix ?id () =
      Format.asprintf "/admin/settings/queue"
      |> append_opt Pool_queue.(map Id.value id)
      |> append_opt suffix
    ;;
  end
end

module Contact = struct
  let experiment_path ?suffix ?id () =
    "/experiments"
    |> append_opt (map Experiment.Id.value id)
    |> append_opt suffix
  ;;
end
