open Tyxml.Html

let format_updated_at value =
  Format.asprintf
    "updated at: %s"
    (value |> Pool_common.UpdatedAt.value |> Utils.Time.ptime_to_date_human)
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
  let languages_html =
    let available_languages = Settings.Language.all () in
    let field_elements =
      CCList.cons
        (Component.csrf_element csrf ())
        (CCList.map
           (fun language ->
             let attrs =
               [ a_input_type `Checkbox
               ; a_name (Settings.Language.code language)
               ]
             in
             let checkbox =
               match
                 CCList.mem
                   ~eq:Settings.Language.equal
                   language
                   tenant_languages
               with
               | false -> input ~a:attrs ()
               | true -> input ~a:(CCList.cons' attrs (a_checked ())) ()
             in
             div [ label [ txt (Settings.Language.code language) ]; checkbox ])
           available_languages)
    in
    div
      [ h2 [ txt "Languages" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/admin/settings/languages")
            ; a_method `Post
            ]
          (field_elements @ [ Component.input_element `Submit None "Save" ])
      ]
  in
  let email_suffixes_html =
    div
      [ h2 [ txt "Email Suffixes" ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path "/admin/settings/email-suffixes")
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
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   "/admin/settings/create-email-suffix")
            ; a_method `Post
            ]
          [ Component.input_element `Text (Some "email_suffix") ""
          ; Component.input_element `Submit None "Add new"
          ]
      ]
  in
  let contact_email_html =
    div
      [ h2 [ txt "Contact Email" ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path "/admin/settings/contact-email")
            ; a_method `Post
            ]
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
            [ a_action
                (Sihl.Web.externalize_path
                   "/admin/settings/inactive_user_disable_after")
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
            [ a_action
                (Sihl.Web.externalize_path
                   "/admin/settings/inactive_user_warning")
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
            [ a_action
                (Sihl.Web.externalize_path
                   "/admin/settings/terms_and_conditions")
            ; a_method `Post
            ]
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
      ]
  in
  Page_layout.create html message ()
;;
