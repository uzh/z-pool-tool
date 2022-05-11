open Tyxml.Html

let detail session Pool_context.{ language; _ } =
  div
    [ h1
        [ txt
            Pool_common.(Utils.text_to_string language I18n.SessionSignUpTitle)
        ]
    ; Page_contact_sessions.public_detail session language
    ]
;;
