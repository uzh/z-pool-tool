open Utils.Lwt_result.Infix
module Translations = I18n
module I18nGuard = I18n.Guard
module NavUtils = Navigation_utils
include Entity

module NavElements = struct
  let read_entity entity =
    Guard.(ValidationSet.one_of_tuple (Permission.Read, entity, None))
  ;;

  let contact_experiment_title context =
    let%lwt experiments_title =
      Translations.find_by_key
        context.Pool_context.database_label
        Translations.Key.ExperimentNavigationTitle
        context.Pool_context.language
      |> Lwt.map Translations.content_to_string
    in
    Lwt.return (I18n.ExperimentsCustom experiments_title)
  ;;

  module Profile = struct
    open I18n

    let prefixed ?(prefix = "") = Format.asprintf "%s%s" prefix
    let nav_link = Profile
    let icon = Icon.Person

    let dropdown_items pool ?(contact = false) () =
      let%lwt show_contact_info = Gtx_config.text_messages_enabled pool in
      (match contact with
       | true ->
         let details = "/user/personal-details", PersonalDetails in
         let contact_info = "/user/contact-information", ContactInformation in
         if show_contact_info then [ details; contact_info ] else [ details ]
       | false -> [])
      |> fun links ->
      [ "/user/login-information", LoginInformation ] @ links |> Lwt.return
    ;;

    let dropdown pool ?contact ?prefix () =
      dropdown_items pool ?contact ()
      ||> CCList.map (fun (url, field) -> Single (prefixed ?prefix url, field, AlwaysOn))
    ;;

    let nav pool ?contact ?prefix () =
      dropdown pool ?contact ?prefix ()
      ||> parent ~validation:AlwaysOn nav_link
      ||> NavElement.create ~icon
    ;;
  end

  module AdminTenantItems = struct
    open I18n

    let dashboard = single "/admin/dashboard" Dashboard AlwaysOn |> NavElement.create

    let settings =
      let children =
        [ parent
            ~url:"/admin/custom-fields"
            ~validation:(Set Custom_field.Guard.Access.index)
            CustomFields
            [ single
                "/admin/custom-fields/settings"
                Settings
                (Set Custom_field.Guard.Access.index)
            ]
        ; single "/admin/filter" Filter (Set Filter.Guard.Access.index)
        ; single "/admin/locations" Locations (Set Pool_location.Guard.Access.index)
        ; single "/admin/settings/queue" Queue (Set (Pool_queue.Guard.Access.index ()))
        ; single "/admin/settings" SystemSettings (Set Settings.Guard.Access.index)
        ; single "/admin/settings/schedules" Schedules (Set Schedule.Guard.Access.index)
        ; single "/admin/settings/smtp" Smtp (Set Email.Guard.Access.Smtp.index)
        ; single
            "/admin/settings/role-permission"
            RolePermissions
            (Set Guard.Access.Permission.read)
        ; single
            "/admin/settings/actor-permission"
            ActorPermissions
            (Set Guard.Access.Permission.read)
        ; single "/admin/settings/tags" Tags (Set Tags.Guard.Access.index)
        ; single
            "/admin/message-template"
            MessageTemplates
            (Set Message_template.Guard.Access.index)
        ; single "/admin/i18n" I18n (Set I18nGuard.Access.index)
        ; single
            "/admin/settings/text-messages"
            TextMessages
            (Set Settings.Guard.Access.update)
        ; single
            Http_utils.Url.Admin.Settings.signup_codes_path
            SignupCodes
            (Set Signup_code.Access.index)
        ; single
            "/admin/organisational-unit"
            OrganisationalUnits
            (Set Organisational_unit.Guard.Access.index)
        ; single (Http_utils.Url.Admin.api_key_path ()) ApiKeys (Set Api_key.Access.index)
        ]
      in
      Parent (None, Settings, OnChildren, children) |> NavElement.create
    ;;

    let user =
      [ single "/admin/contacts" Contacts (Set Contact.Guard.Access.index)
      ; single "/admin/admins" Admins (Set Admin.Guard.Access.index)
      ]
      |> parent Users
      |> NavElement.create
    ;;

    let experiments =
      single "/admin/experiments" Experiments (Set Experiment.Guard.Access.index)
      |> NavElement.create
    ;;

    let all pool =
      let%lwt profile = Profile.nav pool ~prefix:"/admin" () in
      Lwt.return [ dashboard; experiments; settings; user; profile; NavElement.logout () ]
    ;;
  end

  let guest ?(root = false) context =
    let prefix = if root then Some "root" else None in
    [ NavElement.login ?prefix () ] |> NavUtils.create_nav_with_language_switch context
  ;;

  let contact ({ Pool_context.database_label; _ } as context) =
    let%lwt experiments_title = contact_experiment_title context in
    let%lwt profile = Profile.nav database_label ~contact:true () in
    let links =
      [ Single ("/experiments", experiments_title, Set Guard.ValidationSet.empty)
        |> NavElement.create
      ; profile
      ]
    in
    links @ [ NavElement.logout () ]
    |> NavUtils.create_nav_with_language_switch context
    |> Lwt.return
  ;;

  let admin ({ Pool_context.database_label; _ } as context) =
    AdminTenantItems.all database_label
    ||> NavUtils.create_nav ~any_id:true ~validate:true context
  ;;

  let root context =
    let open I18n in
    let tenants =
      single (Http_utils.Url.Root.pool_path ()) Pools (Set Pool_tenant.Guard.Access.index)
      |> NavElement.create
    in
    let users =
      single "/root/users" Users (Set Admin.Guard.Access.index) |> NavElement.create
    in
    let announcements =
      single
        (Http_utils.Url.Root.announcement_path ())
        Announcements
        (Set Announcement.Access.index)
      |> NavElement.create
    in
    let versions =
      single
        (Http_utils.Url.Root.version_path ())
        Versions
        (Set Pool_version.Access.index)
      |> NavElement.create
    in
    let settings =
      [ single "/root/settings/smtp" Smtp (Set Email.Guard.Access.Smtp.index) ]
      |> parent Settings
      |> NavElement.create
    in
    let%lwt profile =
      Profile.nav context.Pool_context.database_label ~prefix:"/root" ()
    in
    [ tenants
    ; users
    ; announcements
    ; versions
    ; settings
    ; profile
    ; NavElement.logout ~prefix:"/root" ()
    ]
    |> NavUtils.create_nav ~any_id:true ~validate:true context
    |> Lwt.return
  ;;

  let find_tenant_nav_links ({ Pool_context.user; _ } as context) languages =
    match user with
    | Pool_context.Guest -> guest context languages |> Lwt.return
    | Pool_context.Contact _ ->
      let%lwt fnc = contact context in
      fnc languages |> Lwt.return
    | Pool_context.Admin _ -> admin context
  ;;

  let find_root_nav_links ({ Pool_context.user; _ } as context) languages =
    match user with
    | Pool_context.Guest | Pool_context.Contact _ ->
      guest ~root:true context languages |> Lwt.return
    | Pool_context.Admin _ -> root context
  ;;
end

let create_main
      ?(kind : [ `Tenant | `Root ] = `Tenant)
      ?active_navigation
      ({ Pool_context.database_label; user; _ } as context)
      tenant_languages
  =
  let%lwt actor = Pool_context.Utils.find_authorizable_opt database_label user in
  let%lwt nav_links =
    let make_links =
      match kind with
      | `Tenant -> NavElements.find_tenant_nav_links
      | `Root -> NavElements.find_root_nav_links
    in
    make_links context tenant_languages
    |> Lwt.map (fun make_links -> make_links ?actor ?active_navigation)
  in
  let desktop = NavUtils.create_desktop_nav nav_links in
  let mobile = NavUtils.make_mobile_header nav_links in
  Lwt.return [ desktop; mobile ]
;;

let create_root ?active_navigation = create_main ~kind:`Root ?active_navigation
