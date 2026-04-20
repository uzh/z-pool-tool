open CCFun.Infix
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
    let of_nav = Utils.nav_link_to_string language in
    let of_field = Utils.field_to_string_capitalized language in
    let externalize =
      Http_utils.url_with_field_params query_parameters %> Sihl.Web.externalize_path
    in
    let fragments =
      let links =
        [ Http_utils.Url.Admin.version_path (), of_nav I18n.Versions
        ; "/credits", of_nav I18n.Credits
        ; "/terms-and-conditions", of_field Pool_message.Field.TermsAndConditions
        ]
      in
      (if privacy_policy_is_set
       then links @ [ "/privacy-policy", of_nav I18n.PrivacyPolicy ]
       else links)
      |> CCList.map (fun (url, label) -> a ~a:[ a_href (externalize url) ] [ txt label ])
    in
    App.create_footer ~app_name:title_text ~fragments () |> Lwt.return
  ;;

  let create
        ?active_navigation
        ({ csrf
         ; language
         ; query_parameters
         ; message
         ; user
         ; notifications
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
        CCOption.map_or ~default:[] (value %> Unsafe.data %> CCList.return)
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
    let announcement = Component.Announcement.from_context language csrf notifications in
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
         ; App.create_footer ()
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
         [ App.navbar [] title_text
         ; content
         ; App.create_footer ()
         ; js_script_tag `IndexJs
         ])
  ;;
end
