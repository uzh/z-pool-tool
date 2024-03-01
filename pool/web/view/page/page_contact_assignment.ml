open Tyxml.Html
open Component.Input

let detail session follow_ups experiment Pool_context.{ language; csrf; _ } =
  let open Pool_common in
  let form_action =
    Format.asprintf
      "/experiments/%s/sessions/%s"
      Experiment.(experiment |> Public.id |> Id.value)
      Session.(Id.value session.Public.id)
    |> Sihl.Web.externalize_path
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.SessionRegistrationTitle) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        (Page_contact_sessions.public_detail language (session :: follow_ups)
         @ [ p
               [ Utils.hint_to_string
                   language
                   (if CCList.is_empty follow_ups
                    then I18n.SessionRegistrationHint
                    else I18n.SessionRegistrationFollowUpHint)
                 |> txt
               ]
           ; form
               ~a:[ a_action form_action; a_method `Post ]
               [ csrf_element csrf ()
               ; div
                   ~a:[ a_class [ "flexrow" ] ]
                   [ submit_element
                       ~classnames:[ "push" ]
                       language
                       Pool_message.Control.Register
                       ~submit_type:`Primary
                       ()
                   ]
               ]
           ])
    ]
;;
