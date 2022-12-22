open Entity
open Message_utils
open Tyxml.Html

let language = Pool_common.Language.De
let entity_uuid = None

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

let add_salutation html = div ((salutation :: html) @ [ complimentary_close ])

let assignment_confirmation =
  let label = Label.AssignmentConfirmation in
  let email_text =
    [ p
        [ txt "Sie wurden erfolgreich zur folgenden Session angemeldet:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ; p [ txt "Die Teilnahme ist obligatorisch." ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Registrierungsbestätigung" |> EmailSubject.of_string in
  let sms_text =
    {|Sie wurden erfolgreich zur folgenden Session angemeldet:

{sessionOverview}

Die Teilnahme ist obligatorisch.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let email_verification =
  let label = Label.EmailVerification in
  let email_text =
    [ p
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Neue E-Mail Adresse" |> EmailSubject.of_string in
  let sms_text =
    {|Du hast kürzlich eine neue E-Mail Adresse zu deinem Account hinzugefügt.

{verificationUrl}

Wenn Sie diese Aktion nicht ausgeführt haben, dann ignorieren Sie diese E-Mail oder trete mit uns in Kontakt.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let experiment_invitation =
  let label = Label.ExperimentInvitation in
  let email_text =
    [ p [ txt "Wir möchten Sie zu einem bevorstehenden Experiment einladen:" ]
    ; p [ txt "{experimentPublicTitle}" ]
    ; p [ txt "{experimentDescription}" ]
    ; p [ txt "Das Experiment wird an folgenden Daten durchgeführt:" ]
    ; p [ txt "Sessions......" ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Einladung zu einem Experiment" |> EmailSubject.of_string
  in
  let sms_text =
    {|Wir möchten Sie zu einem bevorstehenden Experiment einladen:

{experimentPublicTitle}
{experimentDescription}

Das Experiment wird an folgenden Daten durchgeführt:

Sessions......|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let password_change =
  let label = Label.PasswordChange in
  let email_text =
    [ p [ txt "Du hast kürzlich das Passwort deines Accounts geändert." ]
    ; p
        [ txt
            "Wenn du dein Passwort nicht geändert hast, dann kontaktiere uns \
             bitte umgehend."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Neues Passwort" |> EmailSubject.of_string in
  let sms_text =
    {|Du hast kürzlich das Passwort deines Accounts geändert.

Wenn du dein Passwort nicht geändert hast, dann kontaktiere uns bitte umgehend.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let password_reset =
  let label = Label.PasswordReset in
  let email_text =
    [ p
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Passwort zurückgesetzt" |> EmailSubject.of_string in
  let sms_text =
    {|Du hast kürzlich beantragt, dein Passwort für deinen Account zurückzusetzen.

{resetUrl}

Wenn du dies nicht beantragt hast, kannst du diese E-Mail ignorieren oder mit und Kontakt aufnehmen. Der Link ist für die nächste Stunde valid, anschliessend muss ein neuer beantragt werden.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let signup_verification =
  let label = Label.SignUpVerification in
  let email_text =
    [ p
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Registrierungsbestätigung" |> EmailSubject.of_string in
  let sms_text =
    {|Vielen Dank für deine Anmeldung beim Pool Tool.
Nutze den folgenden Link um deinen Account zu aktivieren.

{verificationUrl}

Wenn du dies nicht beantragt hast, kannst du diese E-Mail ignorieren oder mit und Kontakt aufnehmen.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let session_cancellation =
  let label = Label.SessionCancellation in
  let email_text =
    [ p
        [ txt
            "Die folgende Session, zu der du angemeldet warst, wurde abgesagt:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session abgesagt" |> EmailSubject.of_string in
  let sms_text =
    {|Die folgende Session, zu der du angemeldet warst, wurde abgesagt:

{sessionOverview}|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let session_reminder =
  let label = Label.SessionReminder in
  let email_text =
    [ p
        [ txt "Hiermit erinnern wir Sie an die Experiment-Session:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session reminder" |> EmailSubject.of_string in
  let sms_text =
    {|Hiermit erinnern wir Sie an die Experiment-Session

{sessionOverview}|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let session_reschedule =
  let label = Label.SessionReschedule in
  let email_text =
    [ p
        [ txt "Eine Session, zu der Sie angemeldet sind, wurde verschoben:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ; p
        [ txt "Der neue Zeitpunkt ist:"
        ; br ()
        ; txt "{newSessionStart}"
        ; br ()
        ; txt "{newSessionDuration}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Eine Session wurde verschoben" |> EmailSubject.of_string
  in
  let sms_text =
    {|Eine Session, zu der Sie angemeldet sind, wurde verschoben:

{sessionOverview}

Neu:
{newSessionStart}
{newSessionDuration}
|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;
