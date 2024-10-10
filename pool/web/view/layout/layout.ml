open CCFun
open Tyxml.Html
module File = Pool_common.File
module Language = Pool_common.Language
module Message = Message
module Navigation = Navigation
module CustomField = Navigation_custom_fields
module Experiment = Navigation_experiment
module Print = Layout_print

let language_attribute lang =
  Language.show lang |> CCString.lowercase_ascii |> a_lang
;;

module Tenant = struct
  let create
    ?active_navigation
    ({ Pool_context.database_label
     ; language
     ; query_language
     ; message
     ; user
     ; csrf
     ; announcement
     ; _
     } as context)
    Pool_context.Tenant.{ tenant_languages; tenant }
    children
    =
    let open Pool_context in
    let open Pool_tenant in
    let open Layout_utils in
    let title_text = Title.value tenant.title in
    let page_title = title (title_text |> txt) in
    let stylesheets =
      (match tenant.styles with
       | Some _ -> [ `GlobalStylesheet; `TenantStylesheet ]
       | None -> [ `GlobalStylesheet ])
      |> CCList.map css_link_tag
    in
    let scripts =
      (if user_is_admin user then [ `IndexJs; `AdminJs ] else [ `IndexJs ])
      |> CCList.map js_script_tag
    in
    let message = Message.create message language () in
    let htmx_notification =
      div ~a:[ a_id Http_utils.Htmx.notification_id ] []
    in
    let announcement =
      CCOption.map (Component.Announcement.make language csrf) announcement
    in
    let children = div ~a:[ a_class [ "stack" ] ] [ children ] in
    let content =
      main_tag ?announcement [ message; htmx_notification; children ]
    in
    let head_tags =
      let favicon =
        tenant.icon
        |> CCOption.(
             map (Icon.value %> File.externalized_path %> favicon) %> to_list)
      in
      [ charset; viewport ] @ stylesheets @ favicon
    in
    let%lwt navbar_content =
      let title = App.create_title query_language title_text in
      Navigation.create_main ?active_navigation context title tenant_languages
    in
    let%lwt footer =
      let%lwt privacy_policy_is_set =
        I18n.privacy_policy_is_set database_label language
      in
      let open Pool_common in
      let externalize path =
        path
        |> Http_utils.path_with_language query_language
        |> Sihl.Web.externalize_path
      in
      let text_fragments =
        [ txt title_text; txt App.version ]
        |> App.combine_footer_fragments ~classnames:[ "footer-static" ]
      in
      let footer_nav =
        let base =
          [ a
              ~a:[ a_href (externalize "/credits") ]
              [ txt (Utils.nav_link_to_string language I18n.Credits) ]
          ; a
              ~a:[ a_href (externalize "/terms-and-conditions") ]
              [ txt
                  (Utils.field_to_string_capitalized
                     language
                     Pool_message.Field.TermsAndConditions)
              ]
          ]
        in
        let nav_links =
          if privacy_policy_is_set
          then
            base
            @ [ a
                  ~a:[ a_href (externalize "/privacy-policy") ]
                  [ txt (Utils.nav_link_to_string language I18n.PrivacyPolicy) ]
              ]
          else base
        in
        nav_links
        |> App.combine_footer_fragments
             ~column_mobile:true
             ~classnames:[ "footer-nav" ]
      in
      footer
        ~a:
          [ a_class
              [ "inset"
              ; "flexrow"
              ; "flex-gap"
              ; "flexcolumn-mobile"
              ; "justify-center"
              ; "bg-grey-light"
              ; "border-top"
              ; "push"
              ]
          ]
        [ text_fragments
        ; span ~a:[ a_class [ "hidden-mobile" ] ] [ txt "|" ]
        ; footer_nav
        ]
      |> Lwt.return
    in
    html
      ~a:[ language_attribute language ]
      (head page_title head_tags)
      (body
         ~a:[ a_class body_tag_classnames ]
         ([ App.navbar ~children:navbar_content query_language title_text
          ; content
          ; footer
          ]
          @ scripts))
    |> Lwt.return
  ;;
end

module Root = struct
  let create ?active_navigation ({ Pool_context.message; _ } as context) content
    =
    let open Layout_utils in
    let language = Language.En in
    let title_text = App.app_name in
    let page_title = title (txt title_text) in
    let message = Message.create message language () in
    let%lwt navbar_content =
      let title = App.create_title None title_text in
      Navigation.create_root
        ?active_navigation
        Pool_context.{ context with language }
        title
        []
    in
    html
      ~a:[ language_attribute language ]
      (head
         page_title
         [ charset
         ; viewport
         ; favicon (assets `RootFavicon)
         ; `GlobalStylesheet |> css_link_tag
         ])
      (body
         ~a:[ a_class body_tag_classnames ]
         [ App.navbar ~children:navbar_content None title_text
         ; main_tag [ message; content ]
         ; App.root_footer
         ; js_script_tag `IndexJs
         ; js_script_tag `AdminJs
         ])
    |> Lwt.return
  ;;
end

module Error = struct
  let create children =
    let open Layout_utils in
    let title_text = App.app_name in
    let page_title = title (txt title_text) in
    let content = main_tag [ children ] in
    html
      ~a:[ language_attribute Language.En ]
      (head
         page_title
         ([ charset; viewport ] @ [ `GlobalStylesheet |> css_link_tag ]))
      (body
         ~a:[ a_class body_tag_classnames ]
         [ App.navbar None title_text
         ; content
         ; App.root_footer
         ; js_script_tag `IndexJs
         ])
  ;;
end
