open Entity

type default = t list [@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith

let default_values =
  [ ( "confirmation_subject"
    , [ "EN", "Sessionenrollment confirmation"
      ; "DE", "Bestätigung zur Sessionanmeldung"
      ] )
  ; ( "confirmation_text"
    , [ "EN", "We hereby confirm the mandatory assignment."
      ; "DE", "Hiermit bestätigen wir verbindlich Experiment-Teilnahme."
      ] )
  ; ( "confirmation_without_self_registration_subject"
    , [ "EN", "Your expression of interest to the experiment."
      ; "DE", "Ihre Interessensbekundung zum Experiment."
      ] )
  ; ( "confirmation_without_self_registration_text"
    , [ ( "EN"
        , "We have received your expression of interest to the experiment and \
           will contact you shortly." )
      ; ( "DE"
        , "Wir haben Ihre Interessenbekundung für das Experiment erhalten und \
           werden Sie in Kürze kontaktieren." )
      ] )
  ; "credits_text", [ "EN", "<h2>Credits</h2>"; "DE", "<h2>Impressum</h2>" ]
  ; ( "experiment_finish_subject"
    , [ "EN", "Session statements incomplete."
      ; "DE", "Sessionabschluss unvollständig"
      ] )
  ; ( "experiment_finish_text"
    , [ "EN", "Please complete the participants data."
      ; "DE", "Bitte vervollständigen Sie noch die Teilnahmedaten."
      ] )
  ; "greetings_text", [ "EN", "Greetings Text "; "DE", "Begrüssungstext" ]
  ; ( "import_invitation_subject"
    , [ "EN", "Experiment assignment"; "DE", "Experimentteilnahme" ] )
  ; ( "import_invitation_text"
    , [ ( "EN"
        , "We have updated our management software for organizing experiments. \
           Please activate your account." )
      ; ( "DE"
        , "Wir haben unsere Verwaltungssoftware zur Organisation von \
           Experimenten aktualisiert. Bitte aktivieren Sie Ihren Account." )
      ] )
  ; ( "invitation_subject"
    , [ "EN", "Experiment invitation"; "DE", "Einladung zur Studienteilnahme" ]
    )
  ; ( "invitation_text"
    , [ "EN", "We would like to invite you to an upcoming experiment."
      ; "DE", "Wir möchten Sie zu einem bevorstehenden Experiment einladen."
      ] )
  ; ( "invitation_without_self_registration_subject"
    , [ "EN", "Invitation to an experiment"
      ; "DE", "Einladung zu einem Experiment"
      ] )
  ; ( "invitation_without_self_registration_text"
    , [ "EN", "We would like to invite you to an upcoming the experiment."
      ; "DE", "Wir möchten Sie zu einem bevorstehenden Experiment einladen"
      ] )
  ; ( "reminder_sms_text"
    , [ "EN", "Herewith we remind you for the Experiment-Session"
      ; "DE", "Hiermit erinnern wir Sie an die Experiment-Session"
      ] )
  ; ( "reminder_subject"
    , [ "EN", "Reminder: Experiment-Session"
      ; "DE", "Erinnerung: Experiment-Session"
      ] )
  ; ( "reminder_text"
    , [ "EN", "Herewith we remind you for the Experiment-Session"
      ; "DE", "Hiermit erinnern wir Sie an die Experiment-Session"
      ] )
  ; ( "session_finish_subject"
    , [ "EN", "Session statements incomplete."
      ; "DE", "Sessionabschluss unvollständig"
      ] )
  ; ( "session_finish_text"
    , [ "EN", "Please complete the participants data."
      ; "DE", "Bitte vervollständigen Sie noch die Teilnahmedaten."
      ] )
  ; ( "reschedule_session_subject"
    , [ "EN", "Your session was rescheduled."
      ; "DE", "Ihre Session wurde verschoben."
      ] )
  ; ( "reschedule_session_text"
    , [ ( "EN"
        , {|Dear {name}

Your session was rescheduled.

{sessionOverviewEN}

Yours sincerely,
Pool Tool|}
        )
      ; ( "DE"
        , {|Liebe*r {name},

Ihre Session wurde verschoben.

{sessionOverviewDE}

Freundliche Grüsse,
Pool Tool|}
        )
      ] )
  ; ( "trigger_profile_update_subject"
    , [ "EN", "Please check your profile."
      ; "DE", "Bitte kontrollieren Sie Ihr Profil."
      ] )
  ; ( "trigger_profile_update_text"
    , [ ( "EN"
        , {|Dear {name}

Your profile has not been updated in a while.

Yours sincerely,
Pool Tool|}
        )
      ; ( "DE"
        , {|Liebe*r {name},

Ihr Profil wurde bereits eine Weile nicht aktualisiert.

Freundliche Grüsse,
Pool Tool|}
        )
      ] )
  ; "welcome_text", [ "EN", "Welcome"; "DE", "Willkommen" ]
  ; ( "password_policy_text"
    , [ "EN", "Password must be at least 8 characters in length."
      ; "DE", "Das Passwort muss mindestens 8 Zeichen lang sein."
      ] )
  ]
  |> CCList.map (fun (key, data) ->
       let key = key |> Key.of_string |> get_or_failwith in
       CCList.map
         (fun (language, content) ->
           create
             key
             (language |> Pool_common.Language.create |> get_or_failwith)
             (content |> Content.create |> get_or_failwith))
         data)
  |> CCList.flatten
;;
