module Field = Pool_message.Field

let map = CCOption.map
let append suffix path = Format.asprintf "%s/%s" path suffix

let append_opt suffix path =
  suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
;;

let announcement_path ?suffix ?id () =
  ("/" ^ Field.(show Announcement))
  |> append_opt (map Announcement.Id.value id)
  |> append_opt suffix
;;

module Admin = struct
  let admin_path ?suffix ?id () =
    "/admin/admins" |> append_opt (map Admin.Id.value id) |> append_opt suffix
  ;;

  let contact_path ?suffix ?id () =
    "/admin/contacts"
    |> append_opt (map Contact.Id.value id)
    |> append_opt suffix
  ;;

  let custom_fields_path model ?suffix ?id () =
    "/admin/custom-fields"
    |> append (Custom_field.Model.show model)
    |> append "field"
    |> append_opt (map Custom_field.Id.value id)
    |> append_opt suffix
  ;;

  let custom_field_option_path model field_id ?suffix ?id () =
    custom_fields_path model ~id:field_id ()
    |> append "options"
    |> append_opt (map Custom_field.SelectOption.Id.value id)
    |> append_opt suffix
  ;;

  let custom_field_groups_path model ?suffix ?id () =
    "/admin/custom-fields"
    |> append (Custom_field.Model.show model)
    |> append "group"
    |> append_opt (map Custom_field.Group.Id.value id)
    |> append_opt suffix
  ;;

  let experiment_path ?suffix ?id () =
    "/admin/experiments"
    |> append_opt (map Experiment.Id.value id)
    |> append_opt suffix
  ;;

  let filter_path ?suffix ?id () =
    "/admin/filter/" |> append_opt Filter.(map Id.value id) |> append_opt suffix
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

  let user_redirect_path ~id =
    Pool_common.Id.value id |> Format.asprintf "/admin/users/%s"
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
end
