open Tyxml.Html
module Message = Page_message

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

let favicon =
  link
    ~rel:[ `Icon ]
    ~href:(Sihl.Web.externalize_path "/assets/images/favicon.png")
    ()
;;

let global_stylesheets =
  let styles =
    [ ( "https://www.econ.uzh.ch/static/staging/projects/cdn/framework/vlatest/main.css"
      , false )
    ; ( "https://www.econ.uzh.ch/static/staging/projects/cdn/framework/vlatest/font/icons.css"
      , false )
    ; "/assets/index.css", true
    ]
    |> CCList.map (fun (url, externalize) ->
         link
           ~rel:[ `Stylesheet ]
           ~href:(if externalize then Sihl.Web.externalize_path url else url)
           ())
  in
  CCList.cons
    (script
       ~a:
         [ a_src
             "https://www.econ.uzh.ch/static/staging/projects/cdn/framework/vlatest/main.js"
         ; a_defer ()
         ]
       (txt ""))
    styles
;;

let header ?(children = []) title =
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
    [ div [ span ~a:[ a_class [ "heading-2" ] ] [ txt title ] ]; div children ]
;;

let footer title =
  footer
    ~a:
      [ a_class
          [ "inset"
          ; "flexcolumn"
          ; "push"
          ; "align-center"
          ; "bg-grey-light"
          ; "border-top"
          ]
      ]
    [ p [ txt title ] ]
;;

let build_nav_link (url, title) language query_language active_navigation =
  let classnames = [ "nav-link" ] in
  let txt_to_string m =
    Pool_common.Utils.nav_link_to_string language m |> txt
  in
  let nav_link =
    a
      ~a:
        [ a_href (Http_utils.externalize_path_with_lang query_language url)
        ; a_class classnames
        ]
      [ txt_to_string title ]
  in
  active_navigation
  |> CCOption.map_or ~default:nav_link (fun active ->
       if CCString.equal active url
       then
         span
           ~a:[ a_class (CCList.cons "active" classnames) ]
           [ txt_to_string title ]
       else nav_link)
;;

let to_main_nav language query_language active_navigation lst =
  lst
  |> CCList.map (fun item ->
       build_nav_link item language query_language active_navigation)
  |> nav ~a:[ a_class [ "main-nav" ] ]
;;

module Tenant = struct
  let i18n_links tenant_languages active_lang =
    let link_classes = [ "nav-link" ] in
    div
      ~a:[ a_class [ "main-nav" ] ]
      (CCList.map
         (fun tenant_language ->
           let label = Pool_common.Language.show tenant_language in
           if Pool_common.Language.equal tenant_language active_lang
           then
             span
               ~a:[ a_class (CCList.cons "active" link_classes) ]
               [ txt label ]
           else
             a
               ~a:
                 [ a_href
                     Pool_common.(
                       Message.(
                         add_field_query_params
                           ""
                           [ ( Field.Language
                             , Language.show tenant_language
                               |> CCString.lowercase_ascii )
                           ]))
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
    =
    let open Pool_common.I18n in
    let open Pool_context in
    let to_main_nav = to_main_nav language query_language active_navigation in
    let language_switch () = i18n_links tenant_languages active_lang in
    let not_logged_in = [ "/login", Login ] |> to_main_nav in
    let logout = "/logout", Logout in
    let nav_links =
      match user with
      | None -> [ not_logged_in; language_switch () ]
      | Some user ->
        (match user with
         | Admin _ ->
           [ "/admin/dashboard", Dashboard
           ; "/admin/experiments", Experiments
           ; "/admin/custom-fields", CustomFields
           ; "/admin/locations", Locations
           ; "/admin/settings", Settings
           ; "/admin/i18n", I18n
           ; "/admin/contacts", Contacts
           ; "/admin/admins", Admins
           ; logout
           ]
           |> to_main_nav
           |> CCList.pure
         | Contact _ ->
           [ "/experiments", Experiments; "/user", Profile; logout ]
           |> to_main_nav
           |> CCList.pure
         | Root _ -> [ not_logged_in; language_switch () ])
    in
    nav_links
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
    let title_text = Pool_tenant.(Title.value tenant.title) in
    let page_title =
      title (txt (Format.asprintf "%s - %s" title_text "Pool Tool"))
    in
    let custom_stylesheet =
      link
        ~rel:[ `Stylesheet ]
        ~href:(Sihl.Web.externalize_path "/custom/assets/index.css")
        ()
    in
    let message = Message.create message active_lang () in
    let scripts =
      script
        ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
        (txt "")
    in
    let header_content =
      navigation
        user
        active_lang
        query_language
        active_navigation
        tenant_languages
        active_lang
      |> fun nav ->
      nav |> div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] |> CCList.pure
    in
    let content = main_tag [ message; children ] in
    html
      (head
         page_title
         ([ charset; viewport; custom_stylesheet; favicon ] @ global_stylesheets))
      (body
         ~a:[ a_class body_tag_classnames ]
         [ header ~children:header_content title_text
         ; content
         ; footer title_text
         ; scripts
         ])
  ;;
end

let create_root_layout children language message user ?active_navigation () =
  let navigation =
    let to_main_nav =
      to_main_nav Pool_common.Language.En None active_navigation
    in
    let open Pool_common.I18n in
    let open Pool_context in
    let not_logged_in = [ "/root/login", Login ] in
    (match user with
     | None | Some (Contact _) | Some (Admin _) -> not_logged_in
     | Some (Root _) -> [ "/root/tenants", Tenants; "/root/logout", Logout ])
    |> to_main_nav
  in
  let title_text = "Pool Tool" in
  let page_title = title (txt title_text) in
  let message = Message.create message language () in
  let scripts =
    script
      ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
      (txt "")
  in
  let content = main_tag [ message; children ] in
  html
    (head page_title ([ charset; viewport; favicon ] @ global_stylesheets))
    (body
       ~a:[ a_class body_tag_classnames ]
       [ header ~children:[ navigation ] title_text
       ; content
       ; footer title_text
       ; scripts
       ])
;;
