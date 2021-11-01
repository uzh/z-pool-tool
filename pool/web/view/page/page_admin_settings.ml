open Tyxml.Html

let updated_at value =
  Format.asprintf
    "updated at: %s"
    (value |> Pool_common.UpdatedAt.value |> Utils.Time.ptime_to_date_human)
;;

let created_at value =
  Format.asprintf
    "created at: %s"
    (value |> Pool_common.UpdatedAt.value |> Utils.Time.ptime_to_date_human)
;;

let show csrf languages email_suffixes contact_email message () =
  let languages_html =
    let available_languages = Settings.Language.all () in
    let open Settings.TenantLanguages in
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
                   languages.values
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
      ; p [ txt (updated_at languages.updated_at) ]
      ; p [ txt (created_at languages.created_at) ]
      ]
  in
  let email_suffixes_html =
    let open Settings.TenantEmailSuffixes in
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
                 (Some "email suffix")
                 (suffix |> Settings.EmailSuffix.value))
             email_suffixes.values
          @ [ Component.input_element `Submit None "Save" ])
      ; p [ txt (updated_at email_suffixes.updated_at) ]
      ; p [ txt (created_at email_suffixes.created_at) ]
      ]
  in
  let contact_email_html =
    let open Settings.TenantContactEmail in
    div
      [ h2 [ txt "Contact Email" ]
      ; p [ txt (Settings.ContactEmail.value contact_email.value) ]
      ; p [ txt (updated_at contact_email.updated_at) ]
      ; p [ txt (created_at contact_email.created_at) ]
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
