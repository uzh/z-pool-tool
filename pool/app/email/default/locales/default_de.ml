open Entity
open Default_entity
open Default_utils

let salutation =
  let open Tyxml.Html in
  h4 [ txt "Liebe*r {name}," ]
;;

let complimentary_close =
  let open Tyxml.Html in
  p [ txt "Freundliche Grüsse"; br (); txt "Pool Tool" ]
;;

let add_salutation_to_text =
  Format.asprintf "Liebe*r {name},\n\n%s\n\nFreundliche Grüsse\nPool Tool"
;;

let password_reset =
  let label = TemplateLabel.PasswordReset in
  let language = Pool_common.Language.De in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt
            "Du hast kürzlich beantragt, dein Passwort für deinen Account \
             zurückzusetzen."
        ; br ()
        ; txt "Nutze diesen"
        ; a ~a:[ a_href "{resetUrl}" ] [ txt " Link " ]
        ; txt "um es zurückzusetzen."
        ]
    ; p
        [ txt
            "Wenn du dies nicht beantragt hast, kannst du diese E-Mail \
             ignorieren oder mit und Kontakt aufnehmen. Der Link ist für die \
             nächste Stunde valid, anschliessend muss ein neuer beantragt \
             werden."
        ]
    ; p
        [ txt
            "Falls der obige Link nicht funktioniert, kopiere bitte den \
             folgenden manuell in deinen Browser: {resetUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language "Password reset"
    |> html_to_string
  in
  let text =
    {|
Du hast kürzlich beantragt, dein Passwort für deinen Account zurückzusetzen.
Nutze den folgenden Link um es zurückzusetzen.

{resetUrl}

Wenn du dies nicht beantragt hast, kannst du diese E-Mail ignorieren oder mit und Kontakt aufnehmen.
Der Link ist für die nächste Stunde valid, anschliessend muss ein neuer beantragt werden.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let email_verification =
  let label = TemplateLabel.EmailVerification in
  let language = Pool_common.Language.De in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt
            "Du hast kürzlich eine neue E-Mail Adresse zu deinem Account \
             hinzugefügt."
        ; br ()
        ; txt "Nutze diesen"
        ; a ~a:[ a_href "{verificationUrl}" ] [ txt " Link " ]
        ; txt "um sie zu aktivieren."
        ]
    ; p
        [ txt
            "Wenn du diese Aktion nicht ausgeführt hattest, dann ignoriere \
             diese E-Mail oder trete mit uns in Kontakt."
        ]
    ; p
        [ txt
            "Falls der obige Link nicht funktioniert, kopiere bitte den \
             folgenden manuell in deinen Browser: {verificationUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language "Neue E-Mail Adresse"
    |> html_to_string
  in
  let text =
    {|
Du hast kürzlich eine neue E-Mail Adresse zu deinem Account hinzugefügt.
Nutze den folgenden Link um es zurückzusetzen.

{verificationUrl}

Wenn du diese Aktion nicht ausgeführt hattest, dann ignoriere diese E-Mail oder trete mit uns in Kontakt.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let password_change =
  let label = TemplateLabel.PasswordChange in
  let language = Pool_common.Language.De in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p [ txt "Du hast kürzlich das Passwort deines Accounts geändert." ]
    ; p
        [ txt
            "Wenn du dein Passwort nicht geändert hast, dann kontaktiere uns \
             bitte umgehend."
        ]
    ; complimentary_close
    ]
    |> combine_html language "Neues Passwort"
    |> html_to_string
  in
  let text =
    {|
Du hast kürzlich das Passwort deines Accounts geändert.

Wenn du dein Passwort nicht geändert hast, dann kontaktiere uns bitte umgehend.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let signup_verification =
  let label = TemplateLabel.SignUpVerification in
  let language = Pool_common.Language.De in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt "Vielen Dank für deine Anmeldung beim Pool Tool."
        ; br ()
        ; txt "Nutze diesen"
        ; a ~a:[ a_href "{verificationUrl}" ] [ txt " Link " ]
        ; txt "um deinen Account zu aktivieren."
        ]
    ; p
        [ txt
            "Wenn du dies nicht beantragt hast, kannst du diese E-Mail \
             ignorieren oder mit und Kontakt aufnehmen."
        ]
    ; p
        [ txt
            "Falls der obige Link nicht funktioniert, kopiere bitte den \
             folgenden manuell in deinen Browser: {verificationUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language "Sign up"
    |> html_to_string
  in
  let text =
    {|
Vielen Dank für deine Anmeldung beim Pool Tool.
Nutze den folgenden Link um deinen Account zu aktivieren.

{verificationUrl}

Wenn du dies nicht beantragt hast, kannst du diese E-Mail ignorieren oder mit und Kontakt aufnehmen.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;
