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
  p [ txt "Freundliche Grüsse"; br (); txt "{siteTitle}" ]
;;

let add_salutation_to_text =
  Format.asprintf "Liebe*r {name},\n\n%s\n\nFreundliche Grüsse\n{siteTitle}"
;;

let add_salutation html = div ((salutation :: html) @ [ complimentary_close ])

let account_suspension_notification =
  let label = Label.AccountSuspensionNotification in
  let email_text =
    [ p
        [ txt
            "aufgrund zu vieler fehlgeschlagener Anmeldeversuche wurde Ihr \
             Account vorübergehend gesperrt."
        ]
    ; p
        [ txt
            "Wenn diese Versuche nicht von Ihnen durchgeführt wurden, \
             informieren Sie bitte einen Administrator."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Ihr Account wurde temporär gesperrt" |> EmailSubject.of_string
  in
  let sms_text =
    {|Aufgrund zu vieler fehlgeschlagener Anmeldeversuche wurde Ihr Account vorübergehend gesperrt.

Wenn diese Versuche nicht von Ihnen durchgeführt wurden, informieren Sie bitte einen Administrator.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let assignment_confirmation =
  let label = Label.AssignmentConfirmation in
  let email_text =
    [ p
        [ txt
            "Sie wurden erfolgreich zur folgenden Session / zu den folgenden \
             Sessions angemeldet:"
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
    {|Sie wurden erfolgreich zur folgenden Session / zu den folgenden Sessions angemeldet:

{sessionOverview}

Die Teilnahme ist obligatorisch.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let assignment_session_change =
  let label = Label.AssignmentSessionChange in
  let email_text =
    [ p
        [ txt "Sie wurden einer neuen Session zugewiesen:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ; p
        [ txt
            "An der Session vom {oldSessionStart} sind Sie nicht mehr \
             angemeldet."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Sie wurden einer neuen Session zugewiesen" |> EmailSubject.of_string
  in
  let sms_text =
    {|Sie wurden einer neuen Session zugewiesen:

{sessionOverview}

An der Session vom {oldSessionStart} sind Sie nicht mehr angemeldet.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let experiment_invitation =
  let label = Label.ExperimentInvitation in
  let email_text =
    [ p [ txt "Wir möchten Sie zu einem bevorstehenden Experiment einladen:" ]
    ; p [ strong [ txt "{experimentPublicTitle}" ] ]
    ; p [ txt "{experimentDescription}" ]
    ; p
        [ txt "Informationen zu den Sessions finden Sie "
        ; a ~a:[ a_href "{experimentUrl}" ] [ txt "hier" ]
        ; txt "."
        ]
    ; p
        [ txt
            "Falls der obige Link nicht funktioniert, kopiere bitte den \
             folgenden manuell in deinen Browser: {experimentUrl}"
        ]
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

Informationen zu den Sessions finden Sie hier: {experimentUrl}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let phone_verification =
  let label = Label.PhoneVerification in
  let email_text =
    "Ihr Code zur Verifizierung Ihrer Mobiltelefonnummer: {token}"
    |> EmailText.of_string
  in
  let email_subject =
    "Verifizierung Mobiltelefonnummer" |> EmailSubject.of_string
  in
  let sms_text =
    {|Ihr Code zur Verifizierung Ihrer Mobiltelefonnummer: {token}|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let profile_update_trigger =
  let label = Label.ProfileUpdateTrigger in
  let email_text =
    [ p [ txt "Ihr Profil wurde schon länger nicht aktualisiert." ]
    ; p
        [ txt "Bitte kotrollieren Sie die Angaben in Ihrem "
        ; a ~a:[ a_href "{profileUrl}" ] [ txt " Profil " ]
        ; txt "."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Bitte kontrollieren Sie Ihr Profil." |> EmailSubject.of_string
  in
  let sms_text =
    {|Ihr Profil wurde schon länger nicht aktualisiert.

Bitte kontrollieren Sie die Angaben in Ihrem Profil: {profileUrl}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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

Wenn du dies nicht beantragt hast, kannst du diese E-Mail ignorieren oder mit und Kontakt aufnehmen. Der Link ist für die nächste Stunde gültig, anschliessend muss ein neuer beantragt werden.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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
    |> add_salutation_to_text
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let session_cancellation =
  let label = Label.SessionCancellation in
  let email_text =
    [ p
        [ txt
            "Die folgende Session, zu der du angemeldet warst, wurde abgesagt:"
        ]
    ; p [ txt "{sessionOverview}" ]
    ; p [ txt "Grund:" ]
    ; p [ txt "{reason}" ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session abgesagt" |> EmailSubject.of_string in
  let sms_text =
    {|Die folgende Session, zu der du angemeldet warst, wurde abgesagt:

{sessionOverview}

Grund: {reason}
|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
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
        ; txt "{newStart}"
        ; br ()
        ; txt "{newDuration}"
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
{newStart}
{newDuration}
|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let contact_registration_attempt =
  let label = Label.ContactRegistrationAttempt in
  let email_text =
    [ p
        [ txt
            "Es wurde versucht, auf {tenantUrl} ein Konto mit der \
             E-Mail-Adresse '{emailAddress}' zu erstellen."
        ; br ()
        ; txt "Es existiert bereits ein Konto mit dieser E-Mail-Adresse."
        ]
    ; p
        [ txt
            "Wenn Sie dies waren und Ihre Anmeldedaten vergessen haben, können \
             Sie Ihr Passwort "
        ; a ~a:[ a_href "{resetUrl}" ] [ txt "hier" ]
        ; txt "zurücksetzen."
        ]
    ; p
        [ txt
            "Wenn diese Aktion nicht von Ihnen durchgeführt wurde, können Sie \
             diese Meldung ignorieren oder die Administratoren informieren."
        ]
    ; p
        [ txt
            "Wenn der obige Link nicht funktioniert, kopieren Sie bitte den \
             folgenden Link manuell in Ihren Browser: {resetUrl}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Registrierungsversuch" |> EmailSubject.of_string in
  let sms_text =
    {|Es wurde versucht, ein Konto mit der E-Mail-Adresse '{emailAddress}' auf {tenantUrl} zu erstellen.

    Wenn dies Sie waren und Sie Ihre Anmeldedaten vergessen haben, können Sie Ihr Passwort hier zurücksetzen: {resetUrl}

    Wenn diese Aktion nicht von Ihnen durchgeführt wurde, können Sie diese Meldung ignorieren oder die Administratoren informieren.
|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let user_import =
  let label = Label.UserImport in
  let email_text =
    [ p
        [ txt "Ihr Account wurde kürzlich migriert."
        ; br ()
        ; txt "Nutzen Sie diesen"
        ; a ~a:[ a_href "{confirmationUrl}" ] [ txt " Link " ]
        ; txt "um Ihren Account wieder zu aktivieren."
        ]
    ; p
        [ txt
            "Falls der obige Link nicht funktioniert, kopiere bitte den \
             folgenden manuell in deinen Browser: {confirmationUrl}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject =
    "Reaktivierung Ihres Accounts" |> EmailSubject.of_string
  in
  let sms_text =
    {|Ihr Account wurde kürzlich migriert. Nutzen Sie den folgenden Link um Ihren Account wieder zu aktivieren:

{confirmationUrl}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let waiting_list_confirmation =
  let label = Label.WaitingListConfirmation in
  let email_text =
    [ p
        [ txt
            "Sie haben sich erfolgreich auf die Warteliste für das Experiment \
             {experimentPublicTitle} gesetzt."
        ; br ()
        ; txt "Das Recruitment Team wird sich mit Ihnen in Verbindung setzen."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Anmeldung Warteliste" |> EmailSubject.of_string in
  let sms_text =
    {|Sie haben sich erfolgreich auf die Warteliste für das Experiment {experimentPublicTitle} gesetzt.

    Das Recruitment Team wird sich mit Ihnen in Verbindung setzen.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;
