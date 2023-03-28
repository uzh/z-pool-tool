open CCFun
open Tyxml.Html
module CommonUtils = Pool_common.Utils
module LwtResult = Utils.Lwt_result
module I18n = Pool_common.I18n
module Icon = Component.Icon
module Language = Pool_common.Language

module Element = struct
  type t =
    { url : string
    ; label : I18n.nav_link
    ; icon : Component.Icon.t option
    ; validation_set : Guard.ValidationSet.t
    ; children : t list
    }

  let create
    ?icon
    ?(children = [])
    ?(validation_set = Guard.ValidationSet.empty)
    url
    label
    =
    { url; label; icon; validation_set; children }
  ;;

  let create_all =
    CCList.map (fun (url, label, icon, children) ->
      create ~children ?icon url label)
  ;;

  let create_all_req =
    CCList.map (fun (url, label, validation_set) ->
      create ~validation_set url label)
  ;;

  let login = create "/login" I18n.Login
  let logout = create "/logout" I18n.Logout
end

module Desktop = struct
  let create fcn =
    let open Utils.Lwt_result.Infix in
    fcn false ||> div ~a:[ a_class [ "desktop-nav"; "flexrow"; "flex-gap" ] ]
  ;;
end

module Mobile = struct
  let create app_title navigation =
    let open Utils.Lwt_result.Infix in
    let id = "navigation-overlay" in
    let label label =
      Component.Icon.icon label
      |> CCList.pure
      |> div ~a:[ a_user_data "modal" id; a_class [ "icon-lg" ] ]
    in
    let overlay navigation =
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
                navigation
            ]
        ]
    in
    navigation true
    ||> fun items ->
    div
      ~a:[ a_class [ "mobile-nav-wrapper" ] ]
      [ label `MenuOutline; overlay items ]
  ;;
end

module Utils = struct
  let rec build_nav_links
    ?(mobile = false)
    ?active_navigation
    language
    query_language
    { Element.url; label; icon; children; _ }
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
      if is_active || CCList.is_empty children |> not
      then [ span ~a:[ a_class classnames ] label ]
      else
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
          (build_nav_links ~mobile ?active_navigation language query_language)
        %> ul ~a:list_attrs
      in
      nav_link @ [ build_rec children ] |> li ~a:parent_attrs
  ;;

  let create_main
    items
    ?validate
    ?actor
    ?active_navigation
    database_label
    language
    query_language
    mobile
    =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let filtered_items =
      match validate, actor with
      | (None | Some false), _ -> Lwt.return items
      | Some true, Some actor ->
        let rec filter_nav items =
          Lwt_list.filter_map_s
            (fun ({ Element.validation_set; children; _ } as element) ->
              try
                let%lwt self =
                  Persistence.validate database_label validation_set actor
                in
                match self with
                | Ok () when CCList.is_empty children -> Lwt.return_some element
                | Ok () ->
                  let%lwt children = filter_nav children in
                  Lwt.return_some Element.{ element with children }
                | Error _ -> Lwt.return_none
              with
              | _ -> Lwt.return_none)
            items
        in
        filter_nav items
      | _, None -> Lwt.return []
    in
    let%lwt nav_links =
      filtered_items
      ||> CCList.map
            (build_nav_links ~mobile ?active_navigation language query_language)
    in
    let nav = [ nav ~a:[ a_class [ "main-nav" ] ] [ ul nav_links ] ] in
    Lwt.return
    @@
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
    ?actor:_
    ?active_navigation
    database_label
    active_language
    query_language
    mobile
    =
    let open Utils.Lwt_result.Infix in
    let language_switch = i18n_links available_languages active_language in
    let create_main items =
      create_main
        items
        ~validate:false
        ?active_navigation
        database_label
        active_language
        query_language
    in
    create_main elements mobile ||> CCList.cons (language_switch mobile)
  ;;
end

module NavElements = struct
  let read_entity entity =
    Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity entity))
  ;;

  let guest = [ Element.login ] |> Utils.with_language_switch

  let contact =
    let open I18n in
    let profile_dropdown =
      Element.create_all_req
        [ "/user/personal-details", PersonalDetails, Guard.ValidationSet.empty
        ; "/user/login-information", LoginInformation, Guard.ValidationSet.empty
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
      [ "/admin/custom-fields", CustomFields, read_entity `CustomField
      ; "/admin/filter", Filter, read_entity `Filter
      ; "/admin/locations", Locations, read_entity `Location
      ; "/admin/settings", SystemSettings, read_entity `SystemSetting
      ; "/admin/settings/smtp", Smtp, read_entity `Smtp
      ; "/admin/settings/schedules", Schedules, read_entity `Schedule
      ; "/admin/settings/queue", Queue, read_entity `Queue
      ; ( "/admin/message-template"
        , MessageTemplates
        , read_entity `MessageTemplate )
      ; "/admin/i18n", I18n, read_entity `I18n
      ]
      |> Element.create_all_req
      |> fun children ->
      Element.create
        ~validation_set:
          (Guard.ValidationSet.Or
             [ read_entity `CustomField
             ; read_entity `Filter
             ; read_entity `Location
             ; read_entity `SystemSetting
             ; read_entity `Smtp
             ; read_entity `Schedule
             ; read_entity `Queue
             ; read_entity `MessageTemplate
             ; read_entity `I18n
             ])
        ~children
        "/admin/settings"
        Settings
    in
    let user =
      [ "/admin/contacts", Contacts, read_entity `Contact
      ; "/admin/admins", Admins, read_entity `Admin
      ]
      |> Element.create_all_req
      |> fun children ->
      Element.create
        ~validation_set:
          (Guard.ValidationSet.Or [ read_entity `Contact; read_entity `Admin ])
        ~children
        "/admin/users"
        Users
    in
    let dashboard = Element.create "/admin/dashboard" Dashboard in
    let experiments = Element.create "/admin/experiments" Experiments in
    [ dashboard; experiments; settings; user; Element.logout ]
    |> Utils.create_main ~validate:true
  ;;

  let root =
    let open I18n in
    let tenants =
      Element.create
        ~validation_set:(read_entity `Tenant)
        "/root/tenants"
        Tenants
    in
    let users =
      Element.create ~validation_set:(read_entity `Admin) "/root/users" Users
    in
    let settings =
      [ "/root/settings/smtp", Smtp, read_entity `Smtp ]
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
  ?active_navigation
  database_label
  title
  tenant_languages
  query_language
  active_lang
  user
  =
  let open LwtResult.Infix in
  let find_authorizable = function
    | Pool_context.Guest -> Lwt.return_none
    | Pool_context.Contact contact ->
      Contact.id contact
      |> Guard.Uuid.actor_of Pool_common.Id.value
      |> Guard.Persistence.Actor.find database_label `Contact
      ||> CCOption.of_result
    | Pool_context.Admin admin ->
      Admin.id admin
      |> Guard.Uuid.actor_of Admin.Id.value
      |> Guard.Persistence.Actor.find database_label `Admin
      ||> CCOption.of_result
  in
  let%lwt actor = find_authorizable user in
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
  let%lwt desktop = Desktop.create nav_links in
  let%lwt mobile = Mobile.create title nav_links in
  Lwt.return [ desktop; mobile ]
;;

let create_root ?active_navigation = create ~kind:`Root ?active_navigation
