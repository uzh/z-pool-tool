open CCFun
open Tyxml.Html
module File = Pool_common.File
module Language = Pool_common.Language
module Message = Message
module Navigation = Navigation
module CustomField = Navigation_custom_fields
module Experiment = Navigation_experiment
module Print = Layout_print

let language_attribute lang = Language.show lang |> CCString.lowercase_ascii |> a_lang

module Tenant = struct
  open Pool_context
  open Pool_tenant
  open Layout_utils

  let make_footer { database_label; language; query_parameters; _ } title_text =
    let%lwt privacy_policy_is_set = I18n.privacy_policy_is_set database_label language in
    let open Pool_common in
    let externalize path =
      path
      |> Http_utils.url_with_field_params query_parameters
      |> Sihl.Web.externalize_path
    in
    let text_fragments =
      [ txt title_text; txt App.version ] |> App.combine_footer_fragments
    in
    let footer_nav =
      let nav_to_string = Utils.nav_link_to_string language in
      let field_to_string = Utils.field_to_string_capitalized language in
      let links =
        [ Http_utils.Url.Admin.version_path (), nav_to_string I18n.Versions
        ; "/credits", nav_to_string I18n.Credits
        ; "/terms-and-conditions", field_to_string Pool_message.Field.TermsAndConditions
        ]
      in
      let links =
        if privacy_policy_is_set
        then links @ [ "/privacy-policy", nav_to_string I18n.PrivacyPolicy ]
        else links
      in
      links
      |> CCList.map (fun (url, label) -> a ~a:[ a_href (externalize url) ] [ txt label ])
      |> App.combine_footer_fragments ~column_mobile:true ~classnames:[ "footer-nav" ]
    in
    footer
      ~a:
        [ a_class
            [ "inset"
            ; "flexrow"
            ; "flex-gap"
            ; "flexcolumn-mobile"
            ; "justify-center"
            ; "border-top"
            ; "push"
            ]
        ]
      [ text_fragments; span ~a:[ a_class [ "hidden-mobile" ] ] [ txt "|" ]; footer_nav ]
    |> Lwt.return
  ;;

  let create
        ?active_navigation
        ({ csrf
         ; language
         ; query_parameters
         ; message
         ; user
         ; announcement
         ; database_label
         ; _
         } as context)
        Tenant.{ tenant_languages; tenant }
        children
    =
    let title_text = Title.value tenant.title in
    let page_title = title (title_text |> txt) in
    let stylesheets =
      (match tenant.styles with
       | Some _ -> [ `GlobalStylesheet; `TenantStylesheet ]
       | None -> [ `GlobalStylesheet ])
      |> CCList.map css_link_tag
    in
    let%lwt head_script, body_script =
      let open Settings.PageScript in
      let%lwt page_scripts = find database_label in
      let make_script =
        CCOption.map_or ~default:[] (value %> Unsafe.data %> script %> CCList.return)
      in
      let head = make_script page_scripts.head in
      let body = make_script page_scripts.body in
      Lwt.return (head, body)
    in
    let scripts =
      body_script
      @ ((if user_is_admin user then [ `IndexJs; `AdminJs ] else [ `IndexJs ])
         |> CCList.map js_script_tag)
    in
    let message = Message.create message language () in
    let htmx_notification = div ~a:[ a_id Http_utils.Htmx.notification_id ] [] in
    let announcement =
      CCOption.map (Component.Announcement.make language csrf) announcement
    in
    let children = div ~a:[ a_class [ "stack" ] ] [ children ] in
    let content = main_tag ?announcement [ message; htmx_notification; children ] in
    let head_tags =
      let favicon =
        tenant.icon
        |> CCOption.(map (Icon.value %> File.externalized_path %> favicon) %> to_list)
      in
      [ charset; viewport ] @ stylesheets @ favicon @ head_script
    in
    let%lwt navbar_content =
      Navigation.create_main ?active_navigation context tenant_languages
    in
    let%lwt footer = make_footer context title_text in
    html
      ~a:[ language_attribute language ]
      (head page_title head_tags)
      (body
         ~a:[ a_class body_tag_classnames ]
         ([ App.navbar ~children:navbar_content query_parameters title_text
          ; content
          ; footer
          ]
          @ scripts))
    |> Lwt.return
  ;;
end

module Root = struct
  let create
        ?active_navigation
        ({ Pool_context.message; query_parameters; _ } as context)
        content
    =
    let open Layout_utils in
    let language = Language.En in
    let title_text = App.app_name in
    let page_title = title (txt title_text) in
    let message = Message.create message language () in
    let%lwt navbar_content =
      Navigation.create_root ?active_navigation Pool_context.{ context with language } []
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
         [ App.navbar ~children:navbar_content query_parameters title_text
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
      (head page_title ([ charset; viewport ] @ [ `GlobalStylesheet |> css_link_tag ]))
      (body
         ~a:[ a_class body_tag_classnames ]
         [ App.navbar [] title_text; content; App.root_footer; js_script_tag `IndexJs ])
  ;;
end
