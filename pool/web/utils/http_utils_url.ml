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

let version_path ?suffix ?id () =
  ("/" ^ Field.(show Version))
  |> append_opt (map Pool_version.Id.value id)
  |> append_opt suffix
;;

module Admin = struct
  let settings_path = Format.asprintf "/admin/settings/%s"
  let settings_action_path action = action |> Settings.stringify_action |> settings_path

  let system_settings_changelog_path key =
    settings_path "system"
    |> append (Settings.Key.show key)
    |> append Field.(human_url Changelog)
  ;;

  let system_settings_open_changelog_path key =
    system_settings_changelog_path key |> append "open"
  ;;

  let admin_path ?suffix ?id () =
    "/admin/admins" |> append_opt (map Admin.Id.value id) |> append_opt suffix
  ;;

  let api_key_path ?suffix ?id () =
    Field.(human_url ApiKey)
    |> settings_path
    |> append_opt (map Api_key.Id.value id)
    |> append_opt suffix
  ;;

  let contact_path ?suffix ?id () =
    "/admin/contacts" |> append_opt (map Contact.Id.value id) |> append_opt suffix
  ;;

  let contact_duplicate_path contact_id ?suffix ?id () =
    (contact_path ~id:contact_id () ^ "/" ^ Field.(show Duplicate))
    |> append_opt (map Duplicate_contacts.Id.value id)
    |> append_opt suffix
  ;;

  let duplicate_path ?suffix ?id () =
    (contact_path ~suffix:Field.(show Duplicate)) ()
    |> append_opt (map Duplicate_contacts.Id.value id)
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
    "/admin/experiments" |> append_opt (map Experiment.Id.value id) |> append_opt suffix
  ;;

  let filter_path ?suffix ?id () =
    "/admin/filter" |> append_opt Filter.(map Id.value id) |> append_opt suffix
  ;;

  let location_path ?suffix ?id () =
    "/admin/locations" |> append_opt Pool_location.(map Id.value id) |> append_opt suffix
  ;;

  let location_session_path ?suffix id session_id () =
    location_path ~id ()
    |> append (Format.asprintf "sessions/%s" (Session.Id.value session_id))
    |> append_opt suffix
  ;;

  let mailing_path experiment_id ?suffix ?id () =
    experiment_path ~id:experiment_id ~suffix:"mailings" ()
    |> append_opt (map Mailing.Id.value id)
    |> append_opt suffix
  ;;

  let message_template_path ?suffix ?id () =
    Format.asprintf "/admin/%s" Field.(human_url MessageTemplate)
    |> append_opt (map Message_template.Id.value id)
    |> append_opt suffix
  ;;

  let experiment_message_template_path experiment_id label ?suffix ?id () =
    Format.asprintf
      "%s/%s/%s"
      (experiment_path ~id:experiment_id ())
      Field.(human_url MessageTemplate)
      (Message_template.Label.show label)
    |> append_opt (map Message_template.Id.value id)
    |> append_opt suffix
  ;;

  let experiment_user_path id role ?admin_id ?suffix () =
    experiment_path ~id ()
    |> append (Field.show role)
    |> append_opt (map Admin.Id.value admin_id)
    |> append_opt suffix
  ;;

  let organisational_unit_path ?suffix ?id () =
    "/admin/organisational-unit"
    |> append_opt (map Organisational_unit.Id.value id)
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
    Format.asprintf "/admin/experiments/%s/sessions" Experiment.(Id.value experiment_id)
    |> append_opt Session.(map Id.value id)
    |> append_opt suffix
  ;;

  let session_user_path experiment_id id role ?admin_id ?suffix () =
    session_path ~id experiment_id
    |> append (Field.show role)
    |> append_opt (map Admin.Id.value admin_id)
    |> append_opt suffix
  ;;

  let waiting_list_path ?suffix ?id experiment_id =
    experiment_path ~id:experiment_id ~suffix:"waiting-list" ()
    |> append_opt (map Waiting_list.Id.value id)
    |> append_opt suffix
  ;;

  let session_message_template_path experiment_id session_id label ?suffix ?id () =
    Format.asprintf
      "%s/%s/%s"
      (session_path ~id:session_id experiment_id)
      Field.(human_url MessageTemplate)
      (Message_template.Label.show label)
    |> append_opt (map Message_template.Id.value id)
    |> append_opt suffix
  ;;

  let assignment_path experiment_id session_id ?suffix ?id () =
    session_path experiment_id ~id:session_id ~suffix:Field.(human_url Assignments)
    |> append_opt (map Assignment.Id.value id)
    |> append_opt suffix
  ;;

  let user_redirect_path ~id =
    Pool_common.Id.value id |> Format.asprintf "/admin/users/%s"
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

    let tags_path ?id ?suffix () =
      "tags" |> with_settings |> append_opt (map Tags.Id.value id) |> append_opt suffix
    ;;
  end
end

module Contact = struct
  let experiment_path ?suffix ?id () =
    "/experiments" |> append_opt (map Experiment.Id.value id) |> append_opt suffix
  ;;
end

module Root = struct
  let with_root = Format.asprintf "/root%s"
  let announcement_path ?suffix ?id () = announcement_path ?suffix ?id () |> with_root
  let version_path ?suffix ?id () = version_path ?suffix ?id () |> with_root
end
