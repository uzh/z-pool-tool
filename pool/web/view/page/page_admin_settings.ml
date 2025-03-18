open Tyxml.Html
open Component.Input
module Message = Pool_message
module Queue = Page_admin_settings_queue
module ActorPermission = Page_admin_settings_actor_permissions
module RolePermission = Page_admin_settings_permissions
module Schedule = Page_admin_settings_schedule
module Smtp = Page_admin_settings_smtp
module Tags = Page_admin_settings_tags
module TextMessage = Page_admin_settings_text_messages

let changelog_modal_id = "settings-changelog-modal"
let field_to_string = Pool_common.Utils.field_to_string_capitalized

let create_modal context url make_title changelog =
  let open Component in
  Changelog.list context url (Some changelog)
  |> Modal.create ~active:true context.Pool_context.language make_title changelog_modal_id
;;

let settings_changelog_modal context key changelog =
  let url = HttpUtils.Url.Admin.system_settings_changelog_path key |> Uri.of_string in
  let make_title lang = field_to_string lang Field.Changes in
  create_modal context url make_title changelog
;;

let page_scripts_changelog_modal context location changelog =
  let url = HttpUtils.Url.Admin.page_script_changelog_path location |> Uri.of_string in
  let make_title lang =
    let open Pool_message in
    let open Settings.PageScript in
    field_to_string lang
    @@
    match location with
    | Head -> Field.PageScriptsHead
    | Body -> Field.PageScriptsBody
  in
  create_modal context url make_title changelog
;;

let inactive_user_warning_input language warning =
  let open Settings.InactiveUser in
  let remove_btn =
    button
      ~a:
        [ a_class [ "error"; "small" ]
        ; a_button_type `Button
        ; a_onclick "this.parentElement.remove()"
        ]
      [ Component.Icon.(TrashOutline |> to_html) ]
  in
  div
    ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
    [ timespan_picker
        ~classnames:[ "grow" ]
        ~hide_label:true
        ~enabled_time_units:[ Pool_model.Base.TimeUnit.Days ]
        ~required:true
        language
        Pool_message.Field.InactiveUserWarning
        ?value:(CCOption.map Warning.TimeSpan.value warning)
    ; remove_btn
    ]
;;

let show
      ?open_tab
      tenant_languages
      email_suffixes
      contact_email
      inactive_user_disable_after
      inactive_user_warning
      inactive_user_service_disabled
      trigger_profile_update_after
      default_reminder_lead_time
      default_text_msg_reminder_lead_time
      user_import_first_reminder
      user_import_second_reminder
      page_scripts
      Pool_context.{ language; csrf; _ }
      text_messages_enabled
      flash_fetcher
  =
  let submit ?(control = Message.(Control.Update None)) () =
    div
      ~a:[ a_class [ "flexrow" ] ]
      [ submit_element ~classnames:[ "push"; "small" ] language control () ]
  in
  let open_changelog url =
    let open Htmx in
    div
      ~a:[ a_class [ "flexrow"; "justify-end" ] ]
      [ button
          ~a:
            [ hx_get (url |> Sihl.Web.externalize_path)
            ; hx_swap "outerHTML"
            ; hx_target ("#" ^ changelog_modal_id)
            ; a_class [ "small" ]
            ]
          [ txt
              Pool_common.(
                Utils.field_to_string_capitalized language Message.Field.Changelog)
          ]
      ]
  in
  let open_system_settings_changelog key =
    HttpUtils.Url.Admin.system_settings_changelog_path ~suffix:"open" key
    |> open_changelog
  in
  let open_page_scripts_changelog location =
    HttpUtils.Url.Admin.page_script_changelog_path ~suffix:"open" location
    |> open_changelog
  in
  let action_path = HttpUtils.Url.Admin.settings_action_path in
  let form_attrs action =
    [ a_method `Post
    ; a_action (action_path action)
    ; a_class [ "stack" ]
    ; a_user_data "detect-unsaved-changes" ""
    ]
  in
  let make_columns ?hint columns =
    div
      ~a:[ a_class [ "stack" ] ]
      [ hint |> CCOption.map_or ~default:(txt "") (fun hint -> p [ txt hint ])
      ; div ~a:[ a_class [ "grid-col-2"; "flex-gap" ] ] columns
      ]
  in
  let languages_html =
    let all_languages =
      [ tenant_languages |> CCList.map (fun k -> k, true)
      ; Pool_common.Language.all
        |> CCList.filter_map (fun k ->
          match CCList.mem k tenant_languages with
          | true -> None
          | false -> Some (k, false))
      ]
      |> CCList.flatten
    in
    let field_elements =
      CCList.map
        (fun (language, selected) ->
           let attrs =
             [ a_input_type `Checkbox; a_name (Pool_common.Language.show language) ]
           in
           let selected =
             match selected with
             | false -> []
             | true -> [ a_checked () ]
           in
           let checkbox = input ~a:(attrs @ selected) () in
           div [ checkbox; label [ txt (Pool_common.Language.show language) ] ])
        all_languages
      |> Component.Sortable.create_sortable
    in
    let form =
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateLanguages)
            ([ csrf_element csrf (); field_elements ] @ [ submit () ])
        ; open_system_settings_changelog Settings.Key.Languages
        ]
    in
    let hint =
      "You have to add Terms and Condidions before you can activate a new language."
    in
    "Languages", [ form ], Some hint, [ `UpdateLanguages ]
  in
  let email_suffixes_html =
    let control_to_string control =
      h4 [ txt Pool_common.(Utils.control_to_string language control) ]
    in
    let create_form =
      div
        [ control_to_string Message.(Control.Add (Some Field.EmailSuffix))
        ; form
            ~a:(form_attrs `CreateEmailSuffix)
            [ csrf_element csrf ()
            ; div
                ~a:[ a_class [ "stack" ] ]
                [ input_element
                    ~hide_label:true
                    language
                    `Text
                    Pool_message.Field.EmailSuffix
                    ~required:true
                ; submit ~control:Message.(Control.Add None) ()
                ]
            ]
        ]
    in
    let delete_forms suffixes =
      suffixes
      |> CCList.map (fun suffix ->
        form
          ~a:
            [ a_method `Post
            ; a_action (action_path `DeleteEmailSuffix)
            ; a_user_data
                "confirmable"
                Pool_common.(Utils.confirmable_to_string language I18n.DeleteEmailSuffix)
            ]
          [ submit_icon ~classnames:[ "error" ] Icon.TrashOutline
          ; input
              ~a:
                [ a_input_type `Hidden
                ; a_name "email_suffix"
                ; a_value (Settings.EmailSuffix.value suffix)
                ; a_readonly ()
                ]
              ()
          ; csrf_element csrf ()
          ])
      |> div ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
    in
    let update_forms suffixes =
      form
        ~a:
          [ a_method `Post
          ; a_action (action_path `UpdateEmailSuffixes)
          ; a_user_data "detect-unsaved-changes" ""
          ; a_class [ "grow" ]
          ]
        [ csrf_element csrf ()
        ; div
            ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
            (suffixes
             |> CCList.map (fun suffix ->
               input
                 ~a:
                   [ a_value (Settings.EmailSuffix.value suffix)
                   ; a_input_type `Text
                   ; a_name (Pool_message.Field.show Field.EmailSuffix)
                   ; a_required ()
                   ]
                 ()))
        ; div
            ~a:[ a_class [ "flexrow"; "gap" ] ]
            [ submit_element
                ~classnames:[ "push"; "small" ]
                language
                Message.(Control.Update None)
                ()
            ]
        ]
    in
    let suffix_rows = function
      | [] ->
        div
          [ txt Pool_common.(Utils.hint_to_string language I18n.SettingsNoEmailSuffixes) ]
      | suffixes ->
        div
          [ control_to_string Message.(Control.Update (Some Field.EmailSuffix))
          ; div
              ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
              [ update_forms suffixes; delete_forms suffixes ]
          ]
    in
    let title = "Email Suffixes" in
    let columns =
      [ suffix_rows email_suffixes
      ; create_form
      ; div
          ~a:[ a_class [ "full-width" ] ]
          [ open_system_settings_changelog Settings.Key.EmailSuffixes ]
      ]
    in
    title, columns, None, [ `CreateEmailSuffix; `UpdateEmailSuffixes; `DeleteEmailSuffix ]
  in
  let contact_email_html =
    let form =
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateContactEmail)
            [ csrf_element csrf ()
            ; input_element
                language
                `Text
                Pool_message.Field.ContactEmail
                ~value:(contact_email |> Settings.ContactEmail.value)
                ~required:true
            ; submit ~control:Message.(Control.Add None) ()
            ]
        ; open_system_settings_changelog Settings.Key.ContactEmail
        ]
    in
    "Contact Email", [ form ], None, [ `UpdateContactEmail ]
  in
  let inactive_user_html =
    let open Settings.InactiveUser in
    let disable_service_form =
      div
        ~a:[ a_class [ "full-width"; "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateUnactiveUserServiceDisabled)
            [ csrf_element csrf ()
            ; div
                ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                [ checkbox_element
                    ~value:(ServiceDisabled.value inactive_user_service_disabled)
                    language
                    Field.InactiveUserDisableService
                ; submit ()
                ]
            ]
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ open_system_settings_changelog Settings.Key.InactiveUserServiceDisabled ]
        ]
    in
    let disable_after_form =
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateInactiveUserDisableAfter)
            [ csrf_element csrf ()
            ; timespan_picker
                ~required:true
                language
                Pool_message.Field.InactiveUserDisableAfter
                ~value:(inactive_user_disable_after |> DisableAfter.value)
            ; submit ()
            ]
        ; open_system_settings_changelog Settings.Key.InactiveUserDisableAfter
        ]
    in
    let warn_after_form =
      let subforms_id = "inactive-user-warnings" in
      let subforms =
        CCList.map
          (fun warning ->
             warning |> CCOption.return |> inactive_user_warning_input language)
          inactive_user_warning
        |> div ~a:[ a_class [ "stack"; "gap-sm" ]; a_id subforms_id ]
      in
      let buttons =
        div
          ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
          [ button
              ~a:
                Htmx.
                  [ a_button_type `Button
                  ; a_class [ "success"; "small" ]
                  ; hx_get
                      (HttpUtils.Url.Admin.settings_path "inactive-user-warnings"
                       |> Sihl.Web.externalize_path)
                  ; hx_trigger "click"
                  ; hx_target ("#" ^ subforms_id)
                  ; hx_swap "beforeend"
                  ]
              [ txt
                  (Pool_common.Utils.control_to_string
                     language
                     Message.Control.(Add None))
              ]
          ; submit ()
          ]
      in
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateInactiveUserWarning)
            [ csrf_element csrf ()
            ; label
                [ txt
                    (Pool_common.Utils.field_to_string_capitalized
                       language
                       Pool_message.Field.InactiveUserWarning)
                ]
            ; subforms
            ; buttons
            ]
        ; open_system_settings_changelog Settings.Key.InactiveUserWarning
        ]
    in
    let hint = Pool_common.(I18n.SettigsInactiveUsers |> Utils.hint_to_string language) in
    ( "Inactive Users"
    , [ disable_service_form; disable_after_form; warn_after_form ]
    , Some hint
    , [ `UpdateUnactiveUserServiceDisabled; `UpdateInactiveUserDisableAfter ] )
  in
  let trigger_profile_update_after_html =
    let open Settings.TriggerProfileUpdateAfter in
    let title =
      Pool_common.(
        Utils.field_to_string language Pool_message.Field.TriggerProfileUpdateAfter)
      |> CCString.capitalize_ascii
    in
    let form =
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs `UpdateTriggerProfileUpdateAfter)
            [ csrf_element csrf ()
            ; timespan_picker
                ~required:true
                language
                Pool_message.Field.TriggerProfileUpdateAfter
                ~value:(trigger_profile_update_after |> value)
            ; submit ()
            ]
        ; open_system_settings_changelog Settings.Key.TriggerProfileUpdateAfter
        ]
    in
    title, [ form ], None, [ `UpdateTriggerProfileUpdateAfter ]
  in
  let default_lead_time =
    let title = "Reminder lead time" in
    let lead_time_form action key field value encode =
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs action)
            [ csrf_element csrf ()
            ; timespan_picker
                ~value:(value |> encode)
                ~required:true
                ~flash_fetcher
                language
                field
            ; submit ()
            ]
        ; open_system_settings_changelog key
        ]
    in
    let email_lead_time =
      lead_time_form
        `UpdateDefaultLeadTime
        Settings.Key.ReminderLeadTime
        Pool_message.Field.EmailLeadTime
        default_reminder_lead_time
        Pool_common.Reminder.EmailLeadTime.value
    in
    let text_message_lead_time =
      let input_el =
        lead_time_form
          `UpdateTextMsgDefaultLeadTime
          Settings.Key.TextMsgReminderLeadTime
          Pool_message.Field.TextMessageLeadTime
          default_text_msg_reminder_lead_time
          Pool_common.Reminder.TextMessageLeadTime.value
      in
      match text_messages_enabled with
      | true -> input_el
      | false ->
        div
          ~a:[ a_class [ "stack" ] ]
          [ Pool_common.(
              I18n.GtxKeyMissing
              |> Utils.hint_to_string language
              |> txt
              |> CCList.return
              |> Component.Notification.notification language `Warning)
          ; input_el
          ]
    in
    ( title
    , [ email_lead_time; text_message_lead_time ]
    , None
    , [ `UpdateDefaultLeadTime; `UpdateTextMsgDefaultLeadTime ] )
  in
  let user_import_reminder =
    let open Settings.UserImportReminder in
    let enabled_time_units = [ Pool_model.Base.TimeUnit.Days ] in
    let timespan_picker field value =
      timespan_picker
        ~enabled_time_units
        ~min_value:(`Number 1)
        ~required:true
        language
        field
        ~value
    in
    let title = "User import" in
    let columns =
      [ div
          ~a:[ a_class [ "stack" ] ]
          [ form
              ~a:(form_attrs `UserImportFirstReminderAfter)
              [ csrf_element csrf ()
              ; timespan_picker
                  Pool_message.Field.FirstReminder
                  (user_import_first_reminder |> FirstReminderAfter.value)
              ; submit ()
              ]
          ; open_system_settings_changelog Settings.Key.UserImportFirstReminderAfter
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ form
              ~a:(form_attrs `UserImportSecondReminderAfter)
              [ csrf_element csrf ()
              ; timespan_picker
                  Pool_message.Field.SecondReminder
                  (user_import_second_reminder |> SecondReminderAfter.value)
              ; submit ()
              ]
          ; open_system_settings_changelog Settings.Key.UserImportSecondReminderAfter
          ]
      ]
    in
    let hint = Pool_common.(Utils.hint_to_string language I18n.SettingsPageScripts) in
    ( title
    , columns
    , Some hint
    , [ `UserImportFirstReminderAfter; `UserImportSecondReminderAfter ] )
  in
  let page_scripts =
    let open Settings.PageScript in
    let title = "Page scripts" in
    let make_form location =
      let field, script, action =
        match location with
        | Head -> Field.PageScriptsHead, page_scripts.head, `UpdateHeadScripts
        | Body -> Field.PageScriptsBody, page_scripts.body, `UpdateBodyScripts
      in
      div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:(form_attrs action)
            [ csrf_element csrf ()
            ; Component.Input.textarea_element
                ?value:(CCOption.map value script)
                language
                field
            ; submit ()
            ]
        ; open_page_scripts_changelog location
        ]
    in
    let columns =
      [ make_form Settings.PageScript.Head; make_form Settings.PageScript.Body ]
    in
    title, columns, None, [ `UpdateHeadScripts; `UpdateBodyScripts ]
  in
  let body_html =
    [ languages_html
    ; email_suffixes_html
    ; contact_email_html
    ; inactive_user_html
    ; trigger_profile_update_after_html
    ; default_lead_time
    ; user_import_reminder
    ; page_scripts
    ]
    |> CCList.map (fun (title, columns, hint, form_actions) ->
      let html = make_columns ?hint columns in
      let active =
        open_tab
        |> CCOption.map_or ~default:false (fun active_tab ->
          CCList.mem active_tab form_actions)
      in
      Component.Collapsible.create ~active (txt title) html)
    |> div ~a:[ a_class [ "stack" ] ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Settings) ]
    ; div ~a:[ a_id changelog_modal_id; a_class [ "modal"; "fullscreen-overlay" ] ] []
    ; body_html
    ]
;;
