open Tyxml.Html

let detail session experiment Pool_context.{ language; _ } =
  let form_action =
    Format.asprintf
      "/experiments/%s/sessions/%s"
      (experiment.Experiment.Public.id |> Pool_common.Id.value)
      (session.Session.Public.id |> Pool_common.Id.value)
    |> Sihl.Web.externalize_path
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.SessionSignUpTitle)
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Page_contact_sessions.public_detail session language
        ; form
            ~a:[ a_action form_action; a_method `Post ]
            [ Component.submit_element
                language
                Pool_common.Message.(Enroll)
                ~submit_type:`Success
                ()
            ]
        ]
    ]
;;
