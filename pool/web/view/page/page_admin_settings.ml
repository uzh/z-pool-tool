open Tyxml.Html
open Component
module Message = Pool_common.Message

let show
    tenant_languages
    email_suffixes
    contact_email
    inactive_user_disable_after
    inactive_user_warning
    trigger_profile_update_after
    terms_and_conditions
    default_reminder_lead_time
    Pool_context.{ language; csrf; _ }
    flash_fetcher
  =
  let action_path action =
    Sihl.Web.externalize_path
      (Format.asprintf "/admin/settings/%s" (Settings.stringify_action action))
  in
  let form_attrs action =
    [ a_method `Post; a_action (action_path action); a_class [ "stack" ] ]
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
    let terms_and_conditions =
      CCList.map Settings.TermsAndConditions.value terms_and_conditions
    in
    let field_elements =
      div
        ~a:[ a_user_data "sortable" ""; a_class [ "input-group" ] ]
        (CCList.map
           (fun (language, selected) ->
             let attrs =
               [ a_input_type `Checkbox
               ; a_name (Pool_common.Language.show language)
               ]
             in
             let selected =
               match selected with
               | false -> []
               | true -> [ a_checked () ]
             in
             let disabled =
               match
                 CCList.assoc_opt
                   ~eq:Pool_common.Language.equal
                   language
                   terms_and_conditions
               with
               | Some _ -> []
               | None -> [ a_disabled () ]
             in
             let checkbox = input ~a:(attrs @ selected @ disabled) () in
             div
               ~a:[ a_user_data "sortable-item" "" ]
               [ checkbox; label [ txt (Pool_common.Language.show language) ] ])
           all_languages)
    in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Languages" ]
      ; p
          [ txt
              "You have to add Terms and Condidions before you can activate a \
               new language."
          ]
      ; form
          ~a:(form_attrs `UpdateTenantLanguages)
          ([ Component.csrf_element csrf (); field_elements ]
          @ [ submit_element language Message.(Update None) () ])
      ]
  in
  let email_suffixes_html =
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Email Suffixes" ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ form
              ~a:(form_attrs `UpdateTenantEmailSuffixes)
              ([ Component.csrf_element csrf () ]
              @ CCList.map
                  (fun suffix ->
                    Component.input_element
                      language
                      `Text
                      Message.Field.EmailSuffix
                      ~required:true
                      ~value:(suffix |> Settings.EmailSuffix.value))
                  email_suffixes
              @ [ submit_element language Message.(Update None) () ])
          ; form
              ~a:(form_attrs `CreateTenantEmailSuffix)
              [ Component.csrf_element csrf ()
              ; Component.input_element
                  language
                  `Text
                  Message.Field.EmailSuffix
                  ~required:true
              ; submit_element language Message.(Add None) ()
              ]
          ; div
              (CCList.map
                 (fun suffix ->
                   tr
                     [ td [ span [ txt (Settings.EmailSuffix.value suffix) ] ]
                     ; td
                         [ form
                             ~a:
                               [ a_method `Post
                               ; a_action (action_path `DeleteTenantEmailSuffix)
                               ; a_user_data
                                   "confirmable"
                                   Pool_common.(
                                     Utils.confirmable_to_string
                                       language
                                       I18n.DeleteEmailSuffix)
                               ]
                             [ Component.csrf_element csrf ()
                             ; input
                                 ~a:
                                   [ a_input_type `Hidden
                                   ; a_name "email_suffix"
                                   ; a_value (Settings.EmailSuffix.value suffix)
                                   ; a_readonly ()
                                   ]
                                 ()
                             ; submit_element
                                 language
                                 Message.(Delete None)
                                 ~submit_type:`Error
                                 ()
                             ]
                         ]
                     ])
                 email_suffixes
              |> table ~a:[ a_class [ "table" ] ]
              |> CCList.pure)
          ]
      ]
  in
  let contact_email_html =
    div
      [ h2 [ txt "Contact Email" ]
      ; form
          ~a:(form_attrs `UpdateTenantContactEmail)
          [ Component.csrf_element csrf ()
          ; Component.input_element
              language
              `Text
              Message.Field.ContactEmail
              ~value:(contact_email |> Settings.ContactEmail.value)
              ~required:true
          ; submit_element language Message.(Add None) ()
          ]
      ]
  in
  let inactive_user_html =
    let open Settings.InactiveUser in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Inactive Users" ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ form
              ~a:(form_attrs `UpdateInactiveUserDisableAfter)
              [ Component.csrf_element csrf ()
              ; Component.input_element
                  ~help:Pool_common.I18n.NumberIsWeeksHint
                  ~required:true
                  language
                  `Number
                  Message.Field.InactiveUserDisableAfter
                  ~value:
                    (inactive_user_disable_after
                    |> DisableAfter.value
                    |> CCInt.to_string)
              ; submit_element language Message.(Update None) ()
              ]
          ; form
              ~a:(form_attrs `UpdateInactiveUserWarning)
              [ Component.csrf_element csrf ()
              ; Component.input_element
                  ~required:true
                  ~help:Pool_common.I18n.NumberIsDaysHint
                  language
                  `Number
                  Message.Field.InactiveUserWarning
                  ~value:
                    (inactive_user_warning |> Warning.value |> CCInt.to_string)
              ; submit_element language Message.(Update None) ()
              ]
          ]
      ]
  in
  let trigger_profile_update_after_html =
    let open Settings.TriggerProfileUpdateAfter in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ Pool_common.(
              Utils.field_to_string
                language
                Message.Field.TriggerProfileUpdateAfter)
            |> CCString.capitalize_ascii
            |> txt
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ form
              ~a:(form_attrs `UpdateTriggerProfileUpdateAfter)
              [ Component.csrf_element csrf ()
              ; Component.input_element
                  ~help:Pool_common.I18n.NumberIsDaysHint
                  ~required:true
                  language
                  `Number
                  Message.Field.TriggerProfileUpdateAfter
                  ~value:
                    (trigger_profile_update_after |> value |> CCInt.to_string)
              ; submit_element language Message.(Update None) ()
              ]
          ]
      ]
  in
  let terms_and_conditions_html =
    let terms_and_conditions =
      CCList.map Settings.TermsAndConditions.value terms_and_conditions
    in
    let terms_and_conditions_textareas =
      CCList.map
        (fun sys_language ->
          let field =
            let open Pool_common in
            match sys_language with
            | Language.En -> Message.Field.LanguageEn
            | Language.De -> Message.Field.LanguageDe
          in
          Component.textarea_element
            language
            field
            ~value:
              (CCList.assoc_opt
                 ~eq:Pool_common.Language.equal
                 sys_language
                 terms_and_conditions
              |> CCOption.map Settings.TermsAndConditions.Terms.value
              |> CCOption.value ~default:"")
            ~required:true
            ~flash_fetcher)
        Pool_common.Language.all
    in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Terms and conditions" ]
      ; form
          ~a:(form_attrs `UpdateTermsAndConditions)
          ([ Component.csrf_element csrf () ]
          @ terms_and_conditions_textareas
          @ [ submit_element language Message.(Update None) () ])
      ]
  in
  let default_lead_time =
    div
      [ h2 [ txt "Default reminder lead time" ]
      ; form
          ~a:(form_attrs `UpdateDefaultLeadTime)
          [ Component.csrf_element csrf ()
          ; Component.flatpicker_element
              language
              `Time
              Message.Field.LeadTime
              ~value:
                Pool_common.(
                  default_reminder_lead_time
                  |> Reminder.LeadTime.value
                  |> Utils.Time.timespan_spanpicker)
              ~required:true
              ~flash_fetcher
          ; submit_element language Message.(Update None) ()
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt "Settings" ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ languages_html
        ; email_suffixes_html
        ; contact_email_html
        ; inactive_user_html
        ; trigger_profile_update_after_html
        ; terms_and_conditions_html
        ; default_lead_time
        ]
    ; script (Unsafe.data Page_scripts.sortable_js)
    ]
;;
