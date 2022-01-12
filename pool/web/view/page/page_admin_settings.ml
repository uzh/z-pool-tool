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
    csrf
    tenant_languages
    email_suffixes
    contact_email
    inactive_user_disable_after
    inactive_user_warning
    terms_and_conditions
    message
    ()
  =
  let action_path action =
    Sihl.Web.externalize_path
      (Format.asprintf "/admin/settings/%s" (Settings.stringify_action action))
  in
  let languages_html =
    let all_languages =
      [ tenant_languages |> CCList.map (fun k -> k, true)
      ; Pool_common.Language.all ()
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
        ~a:[ a_user_data "sortable" "" ]
        (CCList.map
           (fun (language, selected) ->
             let attrs =
               [ a_input_type `Checkbox
               ; a_name (Pool_common.Language.code language)
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
               [ label [ txt (Pool_common.Language.code language) ]; checkbox ])
           all_languages)
    in
    div
      [ h2 [ txt "Languages" ]
      ; p
          [ txt
              "You have to add Terms and Condidions before you can activate a \
               new language."
          ]
      ; form
          ~a:[ a_action (action_path `UpdateTenantLanguages); a_method `Post ]
          ([ Component.csrf_element csrf (); field_elements ]
          @ [ submit_element Pool_common.Language.En Message.(Update None) ])
      ]
  in
  let email_suffixes_html =
    div
      [ h2 [ txt "Email Suffixes" ]
      ; form
          ~a:
            [ a_action (action_path `UpdateTenantEmailSuffixes)
            ; a_method `Post
            ]
          (CCList.map
             (fun suffix ->
               Component.input_element
                 `Text
                 (Some "email_suffix")
                 (suffix |> Settings.EmailSuffix.value))
             email_suffixes
          @ [ submit_element Pool_common.Language.En Message.(Update None) ])
      ; form
          ~a:[ a_action (action_path `CreateTenantEmailSuffix); a_method `Post ]
          [ Component.input_element `Text (Some "email_suffix") ""
          ; submit_element Pool_common.Language.En Message.(Add None)
          ]
      ; hr ()
      ; div
          (CCList.map
             (fun suffix ->
               form
                 ~a:
                   [ a_action (action_path `DeleteTenantEmailSuffix)
                   ; a_method `Post
                   ]
                 [ span [ txt (Settings.EmailSuffix.value suffix) ]
                 ; input
                     ~a:
                       [ a_input_type `Hidden
                       ; a_name "email_suffix"
                       ; a_value (Settings.EmailSuffix.value suffix)
                       ; a_readonly ()
                       ]
                     ()
                 ; submit_element Pool_common.Language.En Message.(Delete None)
                 ])
             email_suffixes)
      ]
  in
  let contact_email_html =
    div
      [ h2 [ txt "Contact Email" ]
      ; form
          ~a:
            [ a_action (action_path `UpdateTenantContactEmail); a_method `Post ]
          [ Component.input_element
              `Text
              (Some "contact_email")
              (contact_email |> Settings.ContactEmail.value)
          ; submit_element Pool_common.Language.En Message.(Add None)
          ]
      ]
  in
  let inactive_user_html =
    let open Settings.InactiveUser in
    div
      [ h2 [ txt "Inactive Users" ]
      ; form
          ~a:
            [ a_action (action_path `UpdateInactiveUserDisableAfter)
            ; a_method `Post
            ]
          [ p [ txt "Disable user after (weeks)" ]
          ; Component.input_element
              `Number
              (Some "inactive_user_disable_after")
              (inactive_user_disable_after
              |> DisableAfter.value
              |> CCInt.to_string)
          ; submit_element Pool_common.Language.En Message.(Update None)
          ]
      ; form
          ~a:
            [ a_action (action_path `UpdateInactiveUserWarning)
            ; a_method `Post
            ]
          [ p [ txt "Send warning before disabling (days)" ]
          ; Component.input_element
              `Number
              (Some "inactive_user_warning")
              (inactive_user_warning |> Warning.value |> CCInt.to_string)
          ; submit_element Pool_common.Language.En Message.(Update None)
          ]
      ]
  in
  let terms_and_conditions_html =
    let terms_and_conditions =
      CCList.map Settings.TermsAndConditions.value terms_and_conditions
    in
    let terms_and_conditions_textareas =
      CCList.map
        (fun language ->
          div
            [ label [ txt (language |> Pool_common.Language.code) ]
            ; textarea
                ~a:[ a_name (Pool_common.Language.code language) ]
                (txt
                   (CCList.assoc_opt
                      ~eq:Pool_common.Language.equal
                      language
                      terms_and_conditions
                   |> CCOption.map Settings.TermsAndConditions.Terms.value
                   |> Option.value ~default:""))
            ])
        (Pool_common.Language.all ())
    in
    div
      [ h2 [ txt "Terms and conditions" ]
      ; form
          ~a:
            [ a_action (action_path `UpdateTermsAndConditions); a_method `Post ]
          (terms_and_conditions_textareas
          @ [ submit_element Pool_common.Language.En Message.(Update None) ])
      ]
  in
  let html =
    div
      [ h1 [ txt "Settings" ]
      ; languages_html
      ; email_suffixes_html
      ; contact_email_html
      ; inactive_user_html
      ; terms_and_conditions_html
      ; script (Unsafe.data sortable)
      ]
  in
  Page_layout.create html message ()
;;
