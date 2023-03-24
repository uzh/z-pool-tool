open CCFun
open Tyxml.Html
module File = Pool_common.File
module Language = Pool_common.Language
module Navigation = Navigation

let create
  children
  Pool_context.Tenant.{ tenant_languages; tenant }
  ?actor_targets
  ?active_navigation
  ?message
  ?query_language
  active_language
  user
  =
  let open Pool_context in
  let open Pool_tenant in
  let open Layout_utils in
  let title_text = Title.value tenant.title in
  let page_title = title (Format.asprintf "%s - Pool Tool" title_text |> txt) in
  let stylesheets =
    (match tenant.styles with
     | Some _ -> [ `GlobalStylesheet; `TenantStylesheet ]
     | None -> [ `GlobalStylesheet ])
    |> (fun global ->
         if user_is_admin user then global @ [ `AdminStylesheet ] else global)
    |> CCList.map css_link_tag
  in
  let scripts =
    (if user_is_admin user then [ `IndexJs; `AdminJs ] else [ `IndexJs ])
    |> CCList.map js_script_tag
  in
  let message = Message.create message active_language () in
  let content = main_tag [ message; children ] in
  let head_tags =
    let favicon =
      tenant.icon
      |> CCOption.(map (Icon.value %> File.path %> favicon) %> to_list)
    in
    [ charset; viewport ] @ stylesheets @ favicon
  in
  let children =
    let title = App.create_title query_language title_text in
    Navigation.create
      ?actor_targets
      ?active_navigation
      title
      tenant_languages
      query_language
      active_language
      user
  in
  html
    (head page_title head_tags)
    (body
       ~a:[ a_class body_tag_classnames ]
       ([ App.header ~children query_language title_text
        ; content
        ; App.footer title_text
        ]
        @ scripts))
;;

let create_root ?actor_targets ?active_navigation user message content =
  let open Layout_utils in
  let language = Language.En in
  let title_text = "Pool Tool" in
  let page_title = title (txt title_text) in
  let message = Message.create message language () in
  let children =
    let title = App.create_title None title_text in
    Navigation.create_root
      ?actor_targets
      ?active_navigation
      title
      []
      None
      language
      user
  in
  html
    (head
       page_title
       [ charset
       ; viewport
       ; favicon (assets `RootFavicon)
       ; `GlobalStylesheet |> css_link_tag
       ])
    (body
       ~a:[ a_class body_tag_classnames ]
       [ App.header ~children None title_text
       ; main_tag [ message; content ]
       ; App.footer title_text
       ; `IndexJs |> js_script_tag
       ])
;;
