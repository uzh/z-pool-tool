open Tyxml.Html

let format_updated_at value =
  Format.asprintf
    "updated at: %s"
    (value |> Pool_common.UpdatedAt.value |> Utils.Time.ptime_to_date_human)
;;

let show csrf languages email_suffixes contact_email message () =
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
                   (Settings.languages languages)
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
      ; p [ txt (format_updated_at (Settings.updated_at languages)) ]
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
             (Settings.email_suffixes email_suffixes)
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
      ; p [ txt (format_updated_at (Settings.updated_at email_suffixes)) ]
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
              (Settings.contact_email contact_email)
          ; Component.input_element `Submit None "Add new"
          ]
      ; p [ txt (format_updated_at (Settings.updated_at contact_email)) ]
      ]
  in
  let html =
    div
      [ h1 [ txt "Settings" ]
      ; languages_html
      ; email_suffixes_html
      ; contact_email_html
      ]
  in
  Page_layout.create html message ()
;;
