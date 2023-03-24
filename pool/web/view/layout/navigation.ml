open CCFun
open Tyxml.Html
module CommonUtils = Pool_common.Utils
module I18n = Pool_common.I18n
module Icon = Component.Icon
module Language = Pool_common.Language

module Element = struct
  type t =
    { url : string
    ; label : I18n.nav_link
    ; icon : Component.Icon.t option
    ; can_see : Role.Target.t list
    ; children : t list
    }

  let create ?icon ?(children = []) ?(can_see = []) url label =
    { url; label; icon; can_see; children }
  ;;

  let create_all =
    CCList.map (fun (url, label, icon, children) ->
      create ~children ?icon url label)
  ;;

  let create_all_req =
    CCList.map (fun (url, label, can_see) -> create ~can_see url label)
  ;;

  let login = create "/login" I18n.Login
  let logout = create "/logout" I18n.Logout
end

module Desktop = struct
  let create i =
    div ~a:[ a_class [ "desktop-nav"; "flexrow"; "flex-gap" ] ] (i false)
  ;;
end

module Mobile = struct
  let create app_title navigation =
    let id = "navigation-overlay" in
    let label label =
      Component.Icon.icon label
      |> CCList.pure
      |> div ~a:[ a_user_data "modal" id; a_class [ "icon-lg" ] ]
    in
    let overlay =
      div
        ~a:
          [ a_id id
          ; a_class [ "fullscreen-overlay"; "mobile-nav"; "bg-white" ]
          ]
        [ div
            ~a:[ a_class [ "flexcolumn"; "full-height" ] ]
            [ header
                ~a:[ a_class [ "flexrow"; "justify-between"; "align-center" ] ]
                [ app_title; label `Close ]
            ; div
                ~a:[ a_class [ "fade-in"; "inset"; "flexcolumn"; "grow" ] ]
                (navigation true)
            ]
        ]
    in
    div ~a:[ a_class [ "mobile-nav-wrapper" ] ] [ label `MenuOutline; overlay ]
  ;;
end

module Utils = struct
  let rec build_nav_links
    ?(mobile = false)
    ?active_navigation
    ?(validate = false)
    ?(actor_targets = [])
    language
    query_language
    { Element.url; label; icon; can_see; children }
    =
    let rec find_is_active (children : Element.t list_wrap) : bool =
      let is_active url =
        active_navigation |> CCOption.map_or ~default:false (CCString.equal url)
      in
      CCList.fold_left
        (fun init { Element.url; children; _ } ->
          init || is_active url || find_is_active children)
        (is_active url)
        children
    in
    let is_active = find_is_active children in
    let label =
      let label_elt = label |> CommonUtils.nav_link_to_string language |> txt in
      CCOption.map_or
        ~default:[ label_elt ]
        (fun icon ->
          [ span
              ~a:[ a_class [ "has-icon" ] ]
              [ Component.Icon.to_html icon; span [ label_elt ] ]
          ])
        icon
    in
    let nav_link : [< Html_types.li_content_fun ] elt list_wrap =
      let classnames =
        let base = [ "nav-link" ] in
        if is_active then "active" :: base else base
      in
      let hide_nav =
        validate
        && CCList.(
             (* TODO: use Guard functionality *)
             fold_left (fun init x -> init || mem x actor_targets) false can_see)
      in
      match hide_nav, is_active || CCList.is_empty children |> not with
      | true, _ -> []
      | false, true -> [ span ~a:[ a_class classnames ] label ]
      | false, false ->
        [ a
            ~a:
              [ a_href
                  (Http_utils.externalize_path_with_lang query_language url)
              ; a_class classnames
              ]
            label
        ]
    in
    match children with
    | [] -> li nav_link
    | children ->
      let parent_attrs, list_attrs =
        (* NOTE: Desktop UI only supports one nested navigation group *)
        if mobile
        then [], [ a_class [ "children" ] ]
        else [ a_class [ "has-dropdown" ] ], [ a_class [ "dropdown" ] ]
      in
      let build_rec =
        CCList.map
          (build_nav_links
             ~mobile
             ~validate
             ~actor_targets
             ?active_navigation
             language
             query_language)
        %> ul ~a:list_attrs
      in
      nav_link @ [ build_rec children ] |> li ~a:parent_attrs
  ;;

  let create_main
    items
    ?validate
    ?actor_targets
    ?active_navigation
    language
    query_language
    mobile
    =
    let nav_links =
      CCList.map
        (build_nav_links
           ~mobile
           ?validate
           ?actor_targets
           ?active_navigation
           language
           query_language)
        items
    in
    let nav = [ nav ~a:[ a_class [ "main-nav" ] ] [ ul nav_links ] ] in
    if mobile
    then [ div ~a:[ a_class [ "grow"; "flexcolumn"; "justify-center" ] ] nav ]
    else nav
  ;;

  let i18n_links languages active_language mobile =
    let open Pool_common.Message in
    let link_classes = [ "nav-link" ] in
    let nav_class =
      if mobile
      then [ "language-nav"; "gap"; "flexrow"; "flex-gap"; "justify-center" ]
      else [ "main-nav" ]
    in
    let to_html =
      (fun language ->
        let lang = Language.show language in
        if Language.equal language active_language
        then span ~a:[ a_class ("active" :: link_classes) ] [ txt lang ]
        else (
          let query_param =
            [ Field.Language, lang |> CCString.lowercase_ascii ]
          in
          a
            ~a:
              [ a_href (add_field_query_params "" query_param)
              ; a_class link_classes
              ]
            [ txt lang ]))
      |> CCList.map
    in
    languages |> to_html |> nav ~a:[ a_class nav_class ]
  ;;

  let with_language_switch
    elements
    available_languages
    ?actor_targets:_
    ?active_navigation
    active_language
    query_language
    mobile
    =
    let language_switch = i18n_links available_languages active_language in
    let create_main items =
      create_main
        items
        ~validate:false
        ?active_navigation
        active_language
        query_language
    in
    create_main elements mobile @ [ language_switch mobile ]
  ;;
end

module NavElements = struct
  let guest = [ Element.login ] |> Utils.with_language_switch

  let contact =
    let open I18n in
    let profile_dropdown =
      Element.create_all_req
        [ "/user/personal-details", PersonalDetails, []
        ; "/user/login-information", LoginInformation, []
        ]
    in
    [ "/experiments", Experiments, None, []
    ; "/user", Profile, Some Icon.Person, profile_dropdown
    ]
    |> Element.create_all
    |> CCList.cons Element.logout
    |> Utils.with_language_switch
  ;;

  let admin =
    let open I18n in
    let settings =
      [ "/admin/custom-fields", CustomFields, [ `CustomField ]
      ; "/admin/filter", Filter, [ `Filter ]
      ; "/admin/locations", Locations, [ `Location ]
      ; "/admin/settings", SystemSettings, [ `SystemSetting ]
      ; "/admin/settings/smtp", Smtp, [ `Smtp ]
      ; "/admin/settings/schedules", Schedules, [ `Schedule ]
      ; "/admin/settings/queue", Queue, [ `Queue ]
      ; "/admin/message-template", MessageTemplates, [ `MessageTemplate ]
      ; "/admin/i18n", I18n, [ `I18n ]
      ]
      |> Element.create_all_req
      |> fun children ->
      Element.create
        ~can_see:
          [ `CustomField
          ; `Filter
          ; `Location
          ; `SystemSetting
          ; `Smtp
          ; `Schedule
          ; `Queue
          ; `MessageTemplate
          ; `I18n
          ]
        ~children
        "/admin/settings"
        Settings
    in
    let user =
      [ "/admin/contacts", Contacts, [ `Contact ]
      ; "/admin/admins", Admins, [ `Admin ]
      ]
      |> Element.create_all_req
      |> fun children ->
      Element.create ~can_see:[ `Contact ] ~children "/admin/users" Users
    in
    let dashboard = Element.create "/admin/dashboard" Dashboard in
    let experiments = Element.create "/admin/experiments" Experiments in
    [ dashboard; experiments; settings; user; Element.logout ]
    |> Utils.create_main ~validate:true
  ;;

  let root =
    let open I18n in
    let tenants = Element.create ~can_see:[ `Tenant ] "/root/tenants" Tenants in
    let users = Element.create ~can_see:[ `Admin ] "/root/users" Users in
    let settings =
      [ "/root/settings/smtp", Smtp, [ `Smtp ] ]
      |> Element.create_all_req
      |> fun children -> Element.create ~children "/root/settings" Settings
    in
    [ tenants; users; settings; Element.logout ]
    |> Utils.create_main ~validate:true
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
  ?actor_targets
  ?active_navigation
  title
  tenant_languages
  query_language
  active_lang
  user
  =
  let nav_links =
    (match kind with
     | `Tenant -> NavElements.find_tenant_nav_links
     | `Root -> NavElements.find_root_nav_links)
      tenant_languages
      user
      ?actor_targets
      ?active_navigation
      active_lang
      query_language
  in
  [ nav_links |> Desktop.create; nav_links |> Mobile.create title ]
;;

let create_root ?actor_targets ?active_navigation title =
  create ~kind:`Root ?actor_targets ?active_navigation title
;;
