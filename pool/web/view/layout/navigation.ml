module I18nGuard = I18n.Guard
module NavUtils = Navigation_utils
open CCFun
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
      |> CCList.map (fun (url, field) -> prefixed ?prefix url, field)
      |> NavElement.create_all_req
    ;;

    let element ?contact ?prefix () =
      ( prefixed ?prefix "/user"
      , nav_link
      , Some icon
      , dropdown ?contact ?prefix () )
    ;;

    let nav ?contact ?prefix () =
      NavElement.create
        ~icon
        ~children:(dropdown ?contact ?prefix ())
        (prefixed ?prefix "/user")
        nav_link
    ;;
  end

  let guest ?(root = false) context =
    let prefix = if root then Some "root" else None in
    [ NavElement.login ?prefix () ] |> NavUtils.with_language_switch context
  ;;

  let contact context =
    let open I18n in
    let links =
      [ "/experiments", Experiments, None, []
      ; Profile.element ~contact:true ()
      ]
      |> NavElement.create_all
    in
    links @ [ NavElement.logout () ] |> NavUtils.with_language_switch context
  ;;

  let admin context =
    let open I18n in
    let settings =
      [ "/admin/custom-fields", CustomFields, Custom_field.Guard.Access.index
      ; "/admin/filter", Filter, Filter.Guard.Access.index
      ; "/admin/locations", Locations, Pool_location.Guard.Access.index
      ; "/admin/settings/queue", Queue, Queue.Guard.Access.index
      ; "/admin/settings", SystemSettings, Settings.Guard.Access.index
      ; "/admin/settings/schedules", Schedules, Schedule.Guard.Access.index
      ; "/admin/settings/smtp", Smtp, Email.Guard.Access.Smtp.index
      ; ( "/admin/settings/role-permission"
        , RolePermissions
        , Guard.Access.read_permission )
      ; "/admin/settings/tags", Tags, Tags.Guard.Access.index
      ; ( "/admin/message-template"
        , MessageTemplates
        , Message_template.Guard.Access.index )
      ; "/admin/i18n", I18n, I18nGuard.Access.index
      ; ( "/admin/organisational-unit"
        , OrganisationalUnits
        , Organisational_unit.Guard.Access.index )
      ]
      |> NavElement.create_all_req_with_set
      |> fun children ->
      let validation_set =
        CCList.map
          (fun { NavElement.validation_set; _ } -> validation_set)
          children
        |> Guard.ValidationSet.or_
      in
      NavElement.create ~validation_set ~children "/admin/settings" Settings
    in
    let user =
      [ "/admin/contacts", Contacts, Contact.Guard.Access.index
      ; "/admin/admins", Admins, Admin.Guard.Access.index
      ]
      |> NavElement.create_all_req_with_set
      |> fun children ->
      NavElement.create
        ~validation_set:
          (Guard.ValidationSet.Or
             [ Contact.Guard.Access.index; Admin.Guard.Access.index ])
        ~children
        "/admin/users"
        Users
    in
    let dashboard = NavElement.create "/admin/dashboard" Dashboard in
    let experiments =
      NavElement.create
        ~validation_set:Experiment.Guard.Access.index
        "/admin/experiments"
        Experiments
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
      NavElement.create
        ~validation_set:Pool_tenant.Guard.Access.index
        "/root/tenants"
        Tenants
    in
    let users =
      NavElement.create
        ~validation_set:Admin.Guard.Access.index
        "/root/users"
        Users
    in
    let settings =
      [ "/root/settings/smtp", Smtp, Email.Guard.Access.Smtp.index ]
      |> NavElement.create_all_req_with_set
      |> fun children -> NavElement.create ~children "/root/settings" Settings
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
