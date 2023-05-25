open CCFun
open Tyxml.Html
module File = Pool_common.File
module Language = Pool_common.Language
module Message = Message
module Navigation = Navigation
module Experiment = Navigation_experiment

module Tenant = struct
  let create
    children
    Pool_context.Tenant.{ tenant_languages; tenant }
    ?active_navigation
    ?message
    ?query_language
    database_label
    active_language
    user
    =
    let open Pool_context in
    let open Pool_tenant in
    let open Layout_utils in
    let title_text = Title.value tenant.title in
    let page_title =
      title (Format.asprintf "%s - Pool Tool" title_text |> txt)
    in
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
    let htmx_notification = div ~a:[ a_id Http_utils.hx_notification_id ] [] in
    let content = main_tag [ message; htmx_notification; children ] in
    let head_tags =
      let favicon =
        tenant.icon
        |> CCOption.(map (Icon.value %> File.path %> favicon) %> to_list)
      in
      [ charset; viewport ] @ stylesheets @ favicon
    in
    let%lwt children =
      let title = App.create_title query_language title_text in
      Navigation.create
        ?active_navigation
        database_label
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
    |> Lwt.return
  ;;
end

module Root = struct
  let create ?active_navigation ?message database_label user content =
    let open Layout_utils in
    let language = Language.En in
    let title_text = "Pool Tool" in
    let page_title = title (txt title_text) in
    let message = Message.create message language () in
    let%lwt children =
      let title = App.create_title None title_text in
      Navigation.create_root
        ?active_navigation
        database_label
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
         ; js_script_tag `IndexJs
         ])
    |> Lwt.return
  ;;
end

module Error = struct
  let create children =
    let open Layout_utils in
    let title_text = "Pool Tool" in
    let page_title = title (txt title_text) in
    let content = main_tag [ children ] in
    html
      (head
         page_title
         ([ charset; viewport ] @ [ `GlobalStylesheet |> css_link_tag ]))
      (body
         ~a:[ a_class body_tag_classnames ]
         [ App.header None title_text
         ; content
         ; App.footer title_text
         ; js_script_tag `IndexJs
         ])
  ;;
end
