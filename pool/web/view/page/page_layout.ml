open Tyxml.Html
module Message = Page_message
module I18n = Pool_common.I18n
module Language = Pool_common.Language

type nav_element =
  { url : string
  ; label : I18n.nav_link
  ; icon : [ `Person ] option
  ; children : nav_element list option
  }

let create_nav_element ?icon ?children url label =
  { url; label; icon; children }
;;

let logout_nav_link =
  { url = "/logout"; label = I18n.Logout; icon = None; children = None }
;;

let to_nav_elements items =
  items |> CCList.map (fun (url, label) -> create_nav_element url label)
;;

let charset = meta ~a:[ a_charset "utf8" ] ()
let body_tag_classnames = [ "height-100"; "flexcolumn" ]

let main_tag children =
  main [ div ~a:[ a_class [ "inset-xl"; "vertical" ] ] children ]
;;

let viewport =
  meta
    ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
    ()
;;

let favicon path = link ~rel:[ `Icon ] ~href:path ()

let css_link_tag file =
  link
    ~rel:[ `Stylesheet ]
    ~href:(Http_utils.externalized_path_with_version file)
    ()
;;

let global_stylesheet = "/assets/index.css"

let app_title query_language title =
  div
    ~a:[ a_class [ "app-title" ] ]
    [ a
        ~a:
          [ a_href
              (Sihl.Web.externalize_path
                 (Http_utils.path_with_language query_language "/index"))
          ]
        [ txt title ]
    ]
;;

let website_header ?(children = []) query_language title =
  header
    ~a:
      [ a_class
          [ "inset"
          ; "flexrow"
          ; "justify-between"
          ; "align-center"
          ; "bg-grey-light"
          ; "border-bottom"
          ]
      ]
    [ app_title query_language title; div children ]
;;

let desktop_nav elements =
  div ~a:[ a_class [ "desktop-nav"; "flexrow"; "flex-gap" ] ] elements
;;

let mobile_nav query_language title navigation =
  let id = "navigation-overlay" in
  let label label =
    Component.Icon.icon label
    |> CCList.pure
    |> div ~a:[ a_user_data "modal" id; a_class [ "icon-lg" ] ]
  in
  let overlay =
    div
      ~a:[ a_id id; a_class [ "fullscreen-overlay"; "mobile-nav"; "bg-white" ] ]
      [ div
          ~a:[ a_class [ "flexcolumn"; "full-height" ] ]
          [ header
              ~a:[ a_class [ "flexrow"; "justify-between"; "align-center" ] ]
              [ app_title query_language Pool_tenant.(Title.value title)
              ; label `Close
              ]
          ; div
              ~a:[ a_class [ "fade-in"; "inset"; "flexcolumn"; "grow" ] ]
              navigation
          ]
      ]
  in
  div ~a:[ a_class [ "mobile-nav-wrapper" ] ] [ label `MenuOutline; overlay ]
;;

let footer title =
  let version = Version.to_string in
  let title = span [ txt title ] in
  let content = [ title; span [ txt "|" ]; txt version ] in
  footer
    ~a:
      [ a_class
          [ "inset"
          ; "flexrow"
          ; "flex-gap"
          ; "justify-center"
          ; "bg-grey-light"
          ; "border-top"
          ; "push"
          ]
      ]
    content
;;

let rec build_nav_link
  ?(mobile = false)
  language
  query_language
  active_navigation
  { url; label; icon; children }
  =
  (* TODO: add active class to parents of active element *)
  let classnames =
    let base = [ "nav-link" ] in
    active_navigation
    |> CCOption.map_or ~default:base (fun active ->
         if CCString.equal active url then "active" :: base else base)
  in
  let txt_to_string m =
    Pool_common.Utils.nav_link_to_string language m |> txt
  in
  let label =
    match icon with
    | None -> [ txt_to_string label ]
    | Some icon ->
      [ span
          ~a:[ a_class [ "has-icon" ] ]
          [ Component.Icon.icon icon; span [ txt_to_string label ] ]
      ]
  in
  let is_span =
    match active_navigation, children with
    | Some active, _ when CCString.equal active url -> true
    | _, Some children when not (CCList.is_empty children) -> true
    | _, _ -> false
  in
  let nav_link =
    match is_span with
    | false ->
      a
        ~a:
          [ a_href (Http_utils.externalize_path_with_lang query_language url)
          ; a_class classnames
          ]
        label
    | true -> span ~a:[ a_class classnames ] label
  in
  match children with
  | None -> li [ nav_link ]
  | Some children ->
    let parent_attrs, list_attrs =
      match mobile with
      | true -> [], [ a_class [ "children" ] ]
      | false -> [ a_class [ "has-dropdown" ] ], [ a_class [ "dropdown" ] ]
    in
    let children =
      children
      |> CCList.map
           (build_nav_link ~mobile language query_language active_navigation)
      |> ul ~a:list_attrs
    in
    li ~a:parent_attrs [ nav_link; children ]
;;

let to_main_nav ?mobile language query_language active_navigation items =
  let nav =
    items
    |> CCList.map (fun item ->
         build_nav_link ?mobile language query_language active_navigation item)
    |> ul
    |> CCList.pure
    |> nav ~a:[ a_class [ "main-nav" ] ]
  in
  match mobile with
  | Some true ->
    div ~a:[ a_class [ "grow"; "flexcolumn"; "justify-center" ] ] [ nav ]
  | _ -> nav
;;

module Tenant = struct
  let i18n_links tenant_languages active_lang mobile =
    let open Pool_common.Message in
    let link_classes = [ "nav-link" ] in
    let nav_class =
      if mobile
      then [ "language-nav"; "gap"; "flexrow"; "flex-gap"; "justify-center" ]
      else [ "main-nav" ]
    in
    nav
      ~a:[ a_class nav_class ]
      (CCList.map
         (fun tenant_language ->
           let label = Language.show tenant_language in
           if Language.equal tenant_language active_lang
           then
             span
               ~a:[ a_class (CCList.cons "active" link_classes) ]
               [ txt label ]
           else
             a
               ~a:
                 [ a_href
                     (add_field_query_params
                        ""
                        [ ( Field.Language
                          , Language.show tenant_language
                            |> CCString.lowercase_ascii )
                        ])
                 ; a_class link_classes
                 ]
               [ txt label ])
         tenant_languages)
  ;;

  let navigation
    user
    language
    query_language
    active_navigation
    tenant_languages
    active_lang
    title
    =
    let open I18n in
    let open Pool_context in
    let to_main_nav mobile =
      to_main_nav ~mobile language query_language active_navigation
    in
    let language_switch = i18n_links tenant_languages active_lang in
    let not_logged_in mobile =
      [ "/login", Login ] |> to_nav_elements |> to_main_nav mobile
    in
    let profile_dropdown =
      I18n.
        [ "/user/personal-details", PersonalDetails
        ; "/user/login-information", LoginInformation
        ]
      |> to_nav_elements
    in
    let nav_links mobile =
      match user with
      | Guest -> [ not_logged_in mobile; language_switch mobile ]
      | Contact _ ->
        [ [ "/experiments", Experiments, None, None
          ; "/user", Profile, Some `Person, Some profile_dropdown
          ]
          |> CCList.map (fun (url, label, icon, children) ->
               create_nav_element ?icon ?children url label)
          |> CCList.cons logout_nav_link
          |> to_main_nav mobile
        ; language_switch mobile
        ]
      | Admin _ ->
        let settings_nav =
          [ "/admin/custom-fields", CustomFields
          ; "/admin/filter", Filter
          ; "/admin/locations", Locations
          ; "/admin/settings", SystemSettings
          ; "/admin/settings/smtp", Smtp
          ; "/admin/settings/schedules", Schedules
          ; "/admin/settings/queue", Queue
          ; "/admin/message-template", MessageTemplates
          ; "/admin/i18n", I18n
          ]
          |> to_nav_elements
          |> fun children ->
          create_nav_element ~children "/admin/settings" Settings
        in
        let user_nav =
          [ "/admin/contacts", Contacts; "/admin/admins", Admins ]
          |> to_nav_elements
          |> fun children -> create_nav_element ~children "/admin/users" Users
        in
        ([ "/admin/dashboard", Dashboard; "/admin/experiments", Experiments ]
         |> to_nav_elements)
        @ [ settings_nav; user_nav; logout_nav_link ]
        |> CCFun.(to_main_nav mobile %> CCList.pure)
    in
    let desktop_nav = nav_links false |> desktop_nav in
    let mobile_nav = nav_links true |> mobile_nav query_language title in
    [ desktop_nav; mobile_nav ]
  ;;

  let create_layout
    children
    Pool_context.Tenant.{ tenant_languages; tenant }
    user
    message
    active_lang
    query_language
    active_navigation
    =
    let open Pool_context in
    let title_text = Pool_tenant.(Title.value tenant.title) in
    let page_title =
      title (txt (Format.asprintf "%s - %s" title_text "Pool Tool"))
    in
    let stylesheets =
      let global = [ global_stylesheet; "/custom/assets/index.css" ] in
      let files =
        if user_is_admin user
        then CCList.cons "/assets/admin.css" global
        else global
      in
      files |> CCList.map css_link_tag
    in
    let message = Message.create message active_lang () in
    let scripts =
      let global = "index.js" in
      let files =
        if user_is_admin user then [ global; "admin.js" ] else [ global ]
      in
      files
      |> CCList.map (fun file ->
           script
             ~a:
               [ a_src
                   (file
                    |> Format.asprintf "/assets/%s"
                    |> Http_utils.externalized_path_with_version)
               ; a_defer ()
               ]
             (txt ""))
    in
    let header_content =
      navigation
        user
        active_lang
        query_language
        active_navigation
        tenant_languages
        active_lang
        tenant.Pool_tenant.title
    in
    let content = main_tag [ message; children ] in
    let favicon =
      Pool_tenant.(
        tenant.icon |> Icon.value |> Pool_common.File.path |> favicon)
    in
    html
      (head page_title ([ charset; viewport; favicon ] @ stylesheets))
      (body
         ~a:[ a_class body_tag_classnames ]
         ([ website_header ~children:header_content query_language title_text
          ; content
          ; footer title_text
          ]
          @ scripts))
  ;;
end

module Root = struct
  let navigation user active_navigation title =
    let open CCFun in
    let open I18n in
    let open Pool_context in
    let to_main_nav mobile =
      to_main_nav ~mobile Language.En None active_navigation %> CCList.pure
    in
    let not_logged_in = [ "/root/login", Login ] |> to_nav_elements in
    (match user with
     | Contact _ | Guest -> not_logged_in
     | Admin _ ->
       let base_nav : nav_element list =
         [ "/root/tenants", Tenants; "/root/users", Users ] |> to_nav_elements
       in
       let settings_nav : nav_element =
         [ "/root/settings/smtp", Smtp ]
         |> to_nav_elements
         |> fun children ->
         create_nav_element ~children "/root/settings" Settings
       in
       let logout_nav : nav_element list =
         [ "/root/logout", Logout ] |> to_nav_elements
       in
       base_nav @ [ settings_nav ] @ logout_nav)
    |> fun elements ->
    let desktop_nav = to_main_nav false %> desktop_nav in
    let mobile_nav = to_main_nav true %> mobile_nav None title in
    [ desktop_nav elements; mobile_nav elements ]
  ;;

  let create_layout ?active_navigation user message children =
    let title_text = "Pool Tool" in
    let page_title = title (txt title_text) in
    let message = Message.create message Language.En () in
    let scripts =
      script
        ~a:
          [ a_src (Http_utils.externalized_path_with_version "/assets/index.js")
          ; a_defer ()
          ]
        (txt "")
    in
    let content = main_tag [ message; children ] in
    let children =
      navigation user active_navigation (Pool_tenant.Title.of_string title_text)
    in
    html
      (head
         page_title
         ([ charset; viewport; favicon "/assets/images/favicon.png" ]
          @ [ global_stylesheet |> css_link_tag ]))
      (body
         ~a:[ a_class body_tag_classnames ]
         [ website_header None ~children title_text
         ; content
         ; footer title_text
         ; scripts
         ])
  ;;
end

let create_error_layout children =
  let title_text = "Pool Tool" in
  let page_title = title (txt title_text) in
  let scripts =
    script
      ~a:
        [ a_src (Http_utils.externalized_path_with_version "/assets/index.js")
        ; a_defer ()
        ]
      (txt "")
  in
  let content = main_tag [ children ] in
  html
    (head
       page_title
       ([ charset; viewport ] @ [ global_stylesheet |> css_link_tag ]))
    (body
       ~a:[ a_class body_tag_classnames ]
       [ website_header None title_text; content; footer title_text; scripts ])
;;
