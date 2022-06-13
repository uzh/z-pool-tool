open Tyxml.Html
open Component
module Message = Pool_common.Message

let sortable =
  {js|
      function slist (target) {
        target.classList.add("slist");
        var items = target.children, current = null;
        for (let i of items) {
          i.draggable = true;
          i.addEventListener("dragstart", function (e) {
            current = this;
            for (let it of items) {
              if (it != current) { it.classList.add("hint"); }
            }
          });
          i.addEventListener("dragenter", function (e) {
            if (this != current) { this.classList.add("active"); }
          });
          i.addEventListener("dragleave", function () {
            this.classList.remove("active");
          });
          i.addEventListener("dragend", function () {
            for (let it of items) {
              it.classList.remove("hint");
              it.classList.remove("active");
            }
          });
          i.addEventListener("dragover", function (e) {
            e.preventDefault();
          });
          i.addEventListener("drop", function (e) {
            e.preventDefault();
            if (this != current) {
              let currentpos = 0, droppedpos = 0;
              for (let it=0; it<items.length; it++) {
                if (current == items[it]) { currentpos = it; }
                if (this == items[it]) { droppedpos = it; }
              }
              if (currentpos < droppedpos) {
                this.parentNode.insertBefore(current, this.nextSibling);
              } else {
                this.parentNode.insertBefore(current, this);
              }
            }
          });
        }
      }

      window.addEventListener("DOMContentLoaded", function(){
        document.querySelectorAll('[data-sortable]').forEach(elm => {
          slist(elm);
        });
      });
  |js}
;;

let show
    tenant_languages
    email_suffixes
    contact_email
    inactive_user_disable_after
    inactive_user_warning
    terms_and_conditions
    Pool_context.{ language; csrf; _ }
  =
  let action_path action =
    Sihl.Web.externalize_path
      (Format.asprintf "/admin/settings/%s" (Settings.stringify_action action))
  in
  let form_attrs action =
    [ a_method `Post; a_action (action_path action); a_class [ "stack" ] ]
  in
  let input_element = input_element language in
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
                    input_element
                      `Text
                      Message.Field.EmailSuffix
                      (suffix |> Settings.EmailSuffix.value))
                  email_suffixes
              @ [ submit_element language Message.(Update None) () ])
          ; form
              ~a:(form_attrs `CreateTenantEmailSuffix)
              [ Component.csrf_element csrf ()
              ; input_element `Text Message.Field.EmailSuffix ""
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
          ; input_element
              `Text
              Message.Field.ContactEmail
              (contact_email |> Settings.ContactEmail.value)
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
                  language
                  `Number
                  Message.Field.InactiveUserDisableAfter
                  (inactive_user_disable_after
                  |> DisableAfter.value
                  |> CCInt.to_string)
              ; submit_element language Message.(Update None) ()
              ]
          ; form
              ~a:(form_attrs `UpdateInactiveUserWarning)
              [ Component.csrf_element csrf ()
              ; Component.input_element
                  ~help:Pool_common.I18n.NumberIsDaysHint
                  language
                  `Number
                  Message.Field.InactiveUserWarning
                  (inactive_user_warning |> Warning.value |> CCInt.to_string)
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
          Component.textarea_element
            language
            (Pool_common.Language.show sys_language)
            (Pool_common.Language.field_of_t sys_language)
            (CCList.assoc_opt
               ~eq:Pool_common.Language.equal
               sys_language
               terms_and_conditions
            |> CCOption.map Settings.TermsAndConditions.Terms.value
            |> CCOption.value ~default:"")
            ())
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
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt "Settings" ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ languages_html
        ; email_suffixes_html
        ; contact_email_html
        ; inactive_user_html
        ; terms_and_conditions_html
        ]
    ; script (Unsafe.data sortable)
    ]
;;
