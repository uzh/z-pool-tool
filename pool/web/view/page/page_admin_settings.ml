open Tyxml.Html

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
    let available_languages = Pool_common.Language.all () in
    let field_elements =
      div
        ~a:[ a_user_data "sortable" "" ]
        (CCList.map
           (fun language ->
             let attrs =
               [ a_input_type `Checkbox
               ; a_name (Pool_common.Language.code language)
               ]
             in
             let checkbox =
               match
                 CCList.mem
                   ~eq:Pool_common.Language.equal
                   language
                   tenant_languages
               with
               | false -> input ~a:attrs ()
               | true -> input ~a:(CCList.cons' attrs (a_checked ())) ()
             in
             div
               ~a:[ a_user_data "sortable-item" "" ]
               [ label [ txt (Pool_common.Language.code language) ]; checkbox ])
           available_languages)
    in
    div
      [ h2 [ txt "Languages" ]
      ; form
          ~a:[ a_action (action_path `UpdateTenantLanguages); a_method `Post ]
          ([ Component.csrf_element csrf (); field_elements ]
          @ [ Component.input_element `Submit None "Save" ])
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
          @ [ Component.input_element `Submit None "Save" ])
      ; form
          ~a:[ a_action (action_path `CreateTenantEmailSuffix); a_method `Post ]
          [ Component.input_element `Text (Some "email_suffix") ""
          ; Component.input_element `Submit None "Add new"
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
                 ; Component.input_element `Submit None "Delete"
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
          ; Component.input_element `Submit None "Add new"
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
          ; Component.input_element `Submit None "Update"
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
          ; Component.input_element `Submit None "Update"
          ]
      ]
  in
  let terms_and_conditions_html =
    div
      [ h2 [ txt "Terms and conditions" ]
      ; form
          ~a:
            [ a_action (action_path `UpdateTermsAndConditions); a_method `Post ]
          [ textarea
              ~a:[ a_name "terms_and_conditions" ]
              (txt (terms_and_conditions |> Settings.TermsAndConditions.value))
          ; Component.input_element `Submit None "Update"
          ]
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
