let i18n db_pool () =
  let data : (string * (string * string) list) list =
    [ ( "confirmation_subject"
      , [ "EN", "Sessionenrollment confirmation"
        ; "DE", "Bestätigung zur Sessionanmeldung"
        ] )
    ; ( "confirmation_text"
      , [ "EN", "We hereby confirm the mandatory participation."
        ; "DE", "Hiermit bestätigen wir verbindlich Experiment-Teilnahme."
        ] )
    ; ( "confirmation_without_self_registration_subject"
      , [ "EN", "Your expression of interest to the experiment."
        ; "DE", "Ihre Interessensbekundung zum Experiment."
        ] )
    ; ( "confirmation_without_self_registration_text"
      , [ ( "EN"
          , "We have received your expression of interest to the experiment \
             and will contact you shortly." )
        ; ( "DE"
          , "Wir haben Ihre Interessenbekundung für das Experiment erhalten \
             und werden Sie in Kürze kontaktieren." )
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
      , [ "EN", "Experiment participation"; "DE", "Experimentteilnahme" ] )
    ; ( "import_invitation_text"
      , [ ( "EN"
          , "We have updated our management software for organizing \
             experiments. Please activate your account." )
        ; ( "DE"
          , "Wir haben unsere Verwaltungssoftware zur Organisation von \
             Experimenten aktualisiert. Bitte aktivieren Sie Ihren Account." )
        ] )
    ; ( "invitation_subject"
      , [ "EN", "Experiment invitation"
        ; "DE", "Einladung zur Studienteilnahme"
        ] )
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
    ; "welcome_text", [ "EN", "Welcome"; "DE", "Willkommen" ]
    ; ( "password_policy_text"
      , [ "EN", "Password must be at least 6 characters in length."
        ; "DE", "Das Passwort muss mindestens 6 Zeichen lang sein."
        ] )
    ]
  in
  let%lwt () =
    let open CCResult.Infix in
    let all_ok_flatten m = m |> CCList.all_ok >|= CCList.flatten in
    CCList.map
      (fun (key, data) : (Pool_event.t list, Pool_common.Message.error) result ->
        let open Cqrs_command.I18n_command.Create in
        CCList.map
          (fun (language, content) ->
            [ "key", [ key ]; "language", [ language ]; "content", [ content ] ]
            |> decode
            >>= handle)
          data
        |> all_ok_flatten)
      data
    |> all_ok_flatten
    |> function
    | Ok events -> Lwt_list.iter_s (Pool_event.handle_event db_pool) events
    | Error err -> failwith Pool_common.(Utils.error_to_string Language.En err)
  in
  Lwt.return_unit
;;
