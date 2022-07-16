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

  (* TODO[timhub]: * differ between login status *)
  let navigation layout_context language query_language active_navigation =
    let nav_links =
      let open Pool_common.I18n in
      (match layout_context with
      | `Contact -> [ "/experiments", Experiments; "/user", Profile ]
      | `Admin ->
        [ "/admin/dashboard", Dashboard
        ; "/admin/experiments", Experiments
        ; "/admin/locations", Locations
        ; "/admin/settings", Settings
        ; "/admin/i18n", I18n
        ; "/admin/contacts", Contacts
        ; "/admin/admins", Admins
        ])
      @ [ "/logout", Logout ]
      |> CCList.map (fun item ->
             build_nav_link item language query_language active_navigation)
    in
    nav ~a:[ a_class [ "main-nav" ] ] nav_links
  ;;

  let create_layout
      layout_context
      children
      Pool_context.Tenant.{ tenant_languages; tenant }
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
      let navigation =
        navigation layout_context active_lang query_language active_navigation
      in
      (fun html -> [ div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] html ])
      @@
      match layout_context with
      | `Admin -> [ navigation ]
      | `Contact -> [ navigation; i18n_links tenant_languages active_lang ]
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

let create_root_layout children message lang ?active_navigation () =
  (* TODO[timhub]: * differ between login status *)
  let navigation =
    let nav_links =
      let open Pool_common.I18n in
      [ "/root/tenants", Tenants ]
      |> CCList.map (fun item ->
             build_nav_link item Pool_common.Language.En None active_navigation)
    in
    nav ~a:[ a_class [ "main-nav" ] ] nav_links
  in
  let title_text = "Pool Tool" in
  let page_title = title (txt title_text) in
  let message = Message.create message lang () in
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
