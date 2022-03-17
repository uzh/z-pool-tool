open Tyxml.Html
module Message = Page_message

let charset = meta ~a:[ a_charset "utf8" ] ()

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

let global_stylesheet =
  link
    ~rel:[ `Stylesheet ]
    ~href:(Sihl.Web.externalize_path "/assets/index.css")
    ()
;;

let header title lang ?(children = []) () =
  header
    ~a:[ a_class [ "site-header"; "flex-box"; "flex--row"; "flex--between" ] ]
    [ h1 ~a:[ a_style "margin: 0;" ] [ txt title ]
    ; a ~a:[ a_href "?language=de" ] [ txt "Switch lang" ]
    ; div [ txt (Pool_common.Language.code lang) ]
    ; div children
    ]
;;

let footer title =
  footer
    ~a:[ a_class [ "site-footer"; "flex-box"; "flex--row"; "flex--center" ] ]
    [ p [ txt title ] ]
;;

module Tenant = struct
  let create_layout children message lang =
    let update_language tenant_languages =
      form
        ~a:
          [ a_action (Sihl.Web.externalize_path "f/update-language")
          ; a_method `Post
          ]
        [ Component.csrf_element "csrf" ()
        ; select
            ~a:[ a_name "language"; a_onchange "this.form.submit()" ]
            (CCList.map
               (fun l ->
                 let attributes = [ a_value (Pool_common.Language.code l) ] in
                 let attributes =
                   if Pool_common.Language.equal l lang
                   then CCList.cons (a_selected ()) attributes
                   else attributes
                 in
                 option ~a:attributes (txt (Pool_common.Language.code l)))
               tenant_languages)
        ]
    in
    let tenant_languages = Pool_common.Language.[ De; En ] in
    let title_text = "Pool tool" in
    let page_title = title (txt title_text) in
    let custom_stylesheet =
      link
        ~rel:[ `Stylesheet ]
        ~href:(Sihl.Web.externalize_path "/custom/assets/index.css")
        ()
    in
    let message = Message.create message lang () in
    let scripts =
      script
        ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
        (txt "")
    in
    let content = main [ message; children ] in
    html
      (head
         page_title
         [ charset; viewport; custom_stylesheet; global_stylesheet; favicon ])
      (body
         [ header
             title_text
             lang
             ~children:[ update_language tenant_languages ]
             ()
         ; content
         ; footer title_text
         ; scripts
         ])
  ;;
end

let create_root_layout children message lang =
  let title_text = "Pool tool" in
  let page_title = title (txt title_text) in
  let message = Message.create message lang () in
  let scripts =
    script
      ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
      (txt "")
  in
  let content = main ~a:[ a_class [ "site-main" ] ] [ message; children ] in
  html
    (head page_title [ charset; viewport; favicon; global_stylesheet ])
    (body [ header title_text lang (); content; footer title_text; scripts ])
;;
