module Field = Pool_message.Field

let map = CCOption.map

let append_opt suffix path =
  suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
;;

let announcement_path ?suffix ?id () =
  ("/" ^ Field.(show Announcement))
  |> append_opt (map Announcement.Id.value id)
  |> append_opt suffix
;;

let version_path ?suffix ?id () =
  ("/" ^ Field.(show Version))
  |> append_opt (map Pool_version.Id.value id)
  |> append_opt suffix
;;

module Admin = struct
  let settings_path = Format.asprintf "/admin/settings/%s"

  let api_key_path ?suffix ?id () =
    Field.(human_url ApiKey)
    |> settings_path
    |> append_opt (map Api_key.Id.value id)
    |> append_opt suffix
  ;;

  let contact_path ?suffix ?id () =
    "/admin/contacts"
    |> append_opt (map Contact.Id.value id)
    |> append_opt suffix
  ;;

  let contact_duplicate_path contact_id ?suffix ?id () =
    (contact_path ~id:contact_id () ^ "/" ^ Field.(show Duplicate))
    |> append_opt (map Duplicate_contacts.Id.value id)
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

  let version_path ?suffix ?id () =
    version_path ?suffix ?id () |> Format.asprintf "/admin/%s"
  ;;

  module Settings = struct
    let with_settings = Format.asprintf "/admin/settings/%s"

    let queue_list_path ?suffix table =
      let table =
        match table with
        | `Current -> ""
        | `History -> "/archive"
      in
      Format.asprintf "queue%s" table |> with_settings |> append_opt suffix
    ;;

    let queue_path ?suffix ?id () =
      Format.asprintf "queue"
      |> with_settings
      |> append_opt Pool_queue.(map Id.value id)
      |> append_opt suffix
    ;;

    let signup_codes_path = Field.(human_url SignUpCode) |> with_settings
  end
end

module Contact = struct
  let experiment_path ?suffix ?id () =
    "/experiments"
    |> append_opt (map Experiment.Id.value id)
    |> append_opt suffix
  ;;
end

module Root = struct
  let with_root = Format.asprintf "/root%s"

  let announcement_path ?suffix ?id () =
    announcement_path ?suffix ?id () |> with_root
  ;;

  let version_path ?suffix ?id () = version_path ?suffix ?id () |> with_root
end
