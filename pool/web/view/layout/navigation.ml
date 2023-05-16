module I18nGuard = I18n.Guard
module NavUtils = Navigation_utils
open CCFun
open Entity

module NavElements = struct
  let read_entity entity =
    Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity entity))
  ;;

  let guest = [ NavElement.login ] |> NavUtils.with_language_switch

  let contact =
    let open I18n in
    let links =
      let profile_dropdown =
        NavElement.create_all_req
          [ "/user/personal-details", PersonalDetails
          ; "/user/login-information", LoginInformation
          ]
      in
      [ "/experiments", Experiments, None, []
      ; "/user", Profile, Some Icon.Person, profile_dropdown
      ]
      |> NavElement.create_all
    in
    links @ [ NavElement.logout ] |> NavUtils.with_language_switch
  ;;

  let admin =
    let open I18n in
    let settings =
      [ "/admin/custom-fields", CustomFields, Custom_field.Guard.Access.index
      ; "/admin/filter", Filter, Filter.Guard.Access.index
      ; "/admin/locations", Locations, Pool_location.Guard.Access.index
      ; "/admin/settings/queue", Queue, Queue.Guard.Access.index
      ; "/admin/settings", SystemSettings, Settings.Guard.Access.index
      ; "/admin/settings/schedules", Schedules, Schedule.Guard.Access.index
      ; "/admin/settings/smtp", Smtp, Pool_tenant.Guard.Access.Smtp.index
      ; "/admin/settings/rules", Rules, Guard.Access.manage_rules
      ; ( "/admin/message-template"
        , MessageTemplates
        , Message_template.Guard.Access.index )
      ; "/admin/i18n", I18n, I18nGuard.Access.index
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
    [ dashboard; experiments; settings; user; NavElement.logout ]
    |> NavUtils.create_main ~validate:true
  ;;

  let root =
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
      [ "/root/settings/smtp", Smtp, Pool_tenant.Guard.Access.Smtp.index ]
      |> NavElement.create_all_req_with_set
      |> fun children -> NavElement.create ~children "/root/settings" Settings
    in
    [ tenants; users; settings; NavElement.logout ]
    |> NavUtils.create_main ~validate:true
  ;;

  let find_tenant_nav_links languages = function
    | Pool_context.Guest -> guest languages
    | Pool_context.Contact _ -> contact languages
    | Pool_context.Admin _ -> admin
  ;;

  let find_root_nav_links languages = function
    | Pool_context.Guest | Pool_context.Contact _ -> guest languages
    | Pool_context.Admin _ -> root
  ;;
end

let create
  ?(kind : [ `Tenant | `Root ] = `Tenant)
  ?active_navigation
  database_label
  title
  tenant_languages
  query_language
  active_lang
  user
  =
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  let nav_links =
    (match kind with
     | `Tenant -> NavElements.find_tenant_nav_links
     | `Root -> NavElements.find_root_nav_links)
      tenant_languages
      user
      ?actor
      ?active_navigation
      database_label
      active_lang
      query_language
  in
  let%lwt desktop = NavUtils.create_desktop nav_links in
  let%lwt mobile = NavUtils.create_mobile title nav_links in
  Lwt.return [ desktop; mobile ]
;;

let create_root ?active_navigation = create ~kind:`Root ?active_navigation
