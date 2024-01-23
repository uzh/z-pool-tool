module I18nGuard = I18n.Guard
module NavUtils = Navigation_utils
open Entity

module NavElements = struct
  let read_entity entity =
    Guard.(ValidationSet.one_of_tuple (Permission.Read, entity, None))
  ;;

  module Profile = struct
    open I18n

    let prefixed ?(prefix = "") = Format.asprintf "%s%s" prefix
    let nav_link = Profile
    let icon = Icon.Person

    let dropdown ?(contact = false) ?prefix () =
      (if contact
       then
         [ "/user/personal-details", PersonalDetails
         ; "/user/contact-information", ContactInformation
         ]
       else [])
      @ [ "/user/login-information", LoginInformation ]
      |> CCList.map (fun (url, field) ->
        Single (prefixed ?prefix url, field, Guard.ValidationSet.empty))
    ;;

    let element ?contact ?prefix () =
      Parent
        ( prefixed ?prefix "/user"
        , nav_link
        , Guard.ValidationSet.empty
        , dropdown ?contact ?prefix () )
      |> NavElement.create ~icon
    ;;

    let nav ?contact ?prefix () =
      let item =
        Parent
          ( prefixed ?prefix "/user"
          , nav_link
          , Guard.ValidationSet.empty
          , dropdown ?contact ?prefix () )
      in
      NavElement.create ~icon item
    ;;
  end

  let guest ?(root = false) context =
    let prefix = if root then Some "root" else None in
    [ NavElement.login ?prefix () ] |> NavUtils.with_language_switch context
  ;;

  let contact context =
    let open I18n in
    let links =
      [ Single ("/experiments", Experiments, Guard.ValidationSet.empty)
        |> NavElement.create
      ; Profile.element ~contact:true ()
      ]
    in
    links @ [ NavElement.logout () ] |> NavUtils.with_language_switch context
  ;;

  let admin context =
    let open I18n in
    let settings =
      let children =
        [ Parent
            ( "/admin/custom-fields"
            , CustomFields
            , Custom_field.Guard.Access.index
            , [ Single
                  ( "/admin/custom-fields/settings"
                  , Settings
                  , Custom_field.Guard.Access.index )
              ] )
        ; Single ("/admin/filter", Filter, Filter.Guard.Access.index)
        ; Single
            ("/admin/locations", Locations, Pool_location.Guard.Access.index)
        ; Single ("/admin/settings/queue", Queue, Queue.Guard.Access.index)
        ; Single ("/admin/settings", SystemSettings, Settings.Guard.Access.index)
        ; Single
            ("/admin/settings/schedules", Schedules, Schedule.Guard.Access.index)
        ; Single ("/admin/settings/smtp", Smtp, Email.Guard.Access.Smtp.index)
        ; Single
            ( "/admin/settings/role-permission"
            , RolePermissions
            , Guard.Access.read_permission )
        ; Single ("/admin/settings/tags", Tags, Tags.Guard.Access.index)
        ; Single
            ( "/admin/message-template"
            , MessageTemplates
            , Message_template.Guard.Access.index )
        ; Single ("/admin/i18n", I18n, I18nGuard.Access.index)
        ; Single
            ( "/admin/organisational-unit"
            , OrganisationalUnits
            , Organisational_unit.Guard.Access.index )
        ]
      in
      let validation_set =
        CCList.map validation_set children |> Guard.ValidationSet.or_
      in
      Parent ("/admin/settings", Settings, validation_set, children)
      |> NavElement.create
    in
    let user =
      let children =
        [ Single ("/admin/contacts", Contacts, Contact.Guard.Access.index)
        ; Single ("/admin/admins", Admins, Admin.Guard.Access.index)
        ]
      in
      let validation_set =
        Guard.ValidationSet.Or
          [ Contact.Guard.Access.index; Admin.Guard.Access.index ]
      in
      Parent ("/admin/users", Users, validation_set, children)
      |> NavElement.create
    in
    let dashboard =
      Single ("/admin/dashboard", Dashboard, Guard.ValidationSet.empty)
      |> NavElement.create
    in
    let experiments =
      Single ("/admin/experiments", Experiments, Experiment.Guard.Access.index)
      |> NavElement.create
    in
    [ dashboard
    ; experiments
    ; settings
    ; user
    ; Profile.nav ~prefix:"/admin" ()
    ; NavElement.logout ()
    ]
    |> NavUtils.create_main ~validate:true context
  ;;

  let root context =
    let open I18n in
    let tenants =
      Single ("/root/tenants", Tenants, Pool_tenant.Guard.Access.index)
      |> NavElement.create
    in
    let users =
      Single ("/root/users", Users, Admin.Guard.Access.index)
      |> NavElement.create
    in
    let settings =
      let children =
        [ Single ("/root/settings/smtp", Smtp, Email.Guard.Access.Smtp.index) ]
      in
      let validation_set =
        CCList.map validation_set children |> Guard.ValidationSet.or_
      in
      Parent ("/root/settings", Settings, validation_set, children)
      |> NavElement.create
    in
    [ tenants
    ; users
    ; settings
    ; Profile.nav ~prefix:"/root" ()
    ; NavElement.logout ~prefix:"/root" ()
    ]
    |> NavUtils.create_main ~validate:true context
  ;;

  let find_tenant_nav_links ({ Pool_context.user; _ } as context) languages =
    match user with
    | Pool_context.Guest -> guest context languages
    | Pool_context.Contact _ -> contact context languages
    | Pool_context.Admin _ -> admin context
  ;;

  let find_root_nav_links ({ Pool_context.user; _ } as context) languages =
    match user with
    | Pool_context.Guest | Pool_context.Contact _ ->
      guest ~root:true context languages
    | Pool_context.Admin _ -> root context
  ;;
end

let create
  ?(kind : [ `Tenant | `Root ] = `Tenant)
  ?active_navigation
  ({ Pool_context.database_label; user; _ } as context)
  title
  tenant_languages
  =
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  let nav_links =
    (match kind with
     | `Tenant -> NavElements.find_tenant_nav_links
     | `Root -> NavElements.find_root_nav_links)
      context
      tenant_languages
      ?actor
      ?active_navigation
  in
  let%lwt desktop = NavUtils.create_desktop nav_links in
  let%lwt mobile = NavUtils.create_mobile title nav_links in
  Lwt.return [ desktop; mobile ]
;;

let create_root ?active_navigation = create ~kind:`Root ?active_navigation
