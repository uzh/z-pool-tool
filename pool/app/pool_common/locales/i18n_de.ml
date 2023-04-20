open Entity_i18n

let to_string = function
  | Address -> "Addresse"
  | AvailableSpots -> "Freie Plätze"
  | Canceled -> "Abgesagt"
  | ContactWaitingListEmpty -> "Sie sind aktuell auf keiner Warteliste."
  | ContactWaitingListTitle -> "Auf der Warteliste"
  | DashboardProfileCompletionText ->
    "Ihr Profil ist unvollständig. Um zu mehr Experimenten eingeladen zu \
     werden, vervollständigen Sie Ihr Profil."
  | DashboardProfileCompletionTitle -> "Profilvervollständigung"
  | DashboardTitle -> "Dashboard"
  | DeletedAssignments -> "Gelöschte Anmeldungen"
  | DontHaveAnAccount -> "Noch kein Zugang?"
  | EmailConfirmationNote ->
    "Bitte prüfen Sie zunächst Ihre E-Mails und bestätigen Sie Ihre Adresse."
  | EmailConfirmationTitle -> "Bestätigung Ihrer E-Mail Adresse"
  | ExperimentNewTitle -> "Neues Experiment erstellen"
  | ExperimentSessionReminderHint ->
    "Dies sind Standardeinstellungen für die Sessions dieses Experiment. Diese \
     Einstellungen können pro Session angepasst werden."
  | EmtpyList field ->
    Format.asprintf
      "Es sind keine %s vorhanden."
      (Locales_de.field_to_string field)
  | ExperimentContactEnrolledNote ->
    "Sie sind an der/den folgenden Session(s) angemeldet:"
  | Files -> "Dateien"
  | FilterNrOfContacts ->
    "Anzahl der Kontakte, die den Kriterien dieses Filters entsprechen:"
  | FollowUpSessionFor -> "Folgesession für:"
  | ExperimentListTitle -> "Experimente"
  | ExperimentListEmpty ->
    "Aktuell gibt es keine Experimente an den Sie teilnehmen können."
  | ExperimentListPublicTitle -> "Neuanmeldung zu Experiment-Sessions"
  | ExperimentWaitingListTitle -> "Warteliste"
  | HomeTitle -> "Universitäre Anmeldestelle für Studienteilnehmende"
  | I18nTitle -> "Übersetzungen"
  | NoEntries field ->
    Format.asprintf
      "Es existiert noch keine %s."
      (Locales_de.field_to_string field)
  | NotifyVia -> "Benachrichtigen via"
  | OurPartners -> "Unsere Partner"
  | ProfileCompletionText ->
    {|Die folgenden Angaben werden benötigt, um an Experimente eingeladen werden zu können. Weitere Angaben können anschliessend in Ihrem Profil gemacht werden.

Sie kommen für mehr Experimente in Frage, umso kompletter Ihr Profil ist.|}
  | LocationFileNew -> "Neue Datei zu Standort hinzufügen"
  | LocationListTitle -> "Standorte"
  | LocationNewTitle -> "Neuer Standort erstellen"
  | LocationNoFiles -> "Es existieren keine Dateien zu diesem Standort."
  | LocationNoSessions -> "Keine Sessions für diesen Standort gefunden."
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Versand vom %s" (Utils_time.formatted_date_time start)
  | MailingExperimentSessionFullyBooked ->
    "Alle Sessions sind ausgebucht. Es werden keine Einladungen versendet \
     (unabhängig ob z.Z. Mailings aktiv sind).\n\n\
     Füge zusätzliche Sessions zum Experiment hinzu."
  | MailingNewTitle -> "Neuen Versand erstellen"
  | RateTotalSent number ->
    Format.asprintf "Total generierter Einladungen: %d" number
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | Reminder -> "Erinnerung"
  | RoleApplicableToAssign -> "Zuweisbare Benutzer"
  | RoleCurrentlyAssigned -> "Aktuell zugewiesen"
  | RoleCurrentlyNoneAssigned field ->
    Format.asprintf
      "Aktuell sind keine %s zugewiesen."
      (Locales_de.field_to_string field)
  | RolesGranted -> "Zugewiesene Rollen"
  | SentInvitations -> "Versendete Einladungen"
  | SessionDetailTitle start ->
    Format.asprintf "Session am %s" (Utils_time.formatted_date_time start)
  | SessionIndent -> "Einrückungen groupieren Folgesessions."
  | SessionReminderDefaultLeadTime leadtime ->
    Format.asprintf
      "Die Standardvorlaufzeit ist: %s"
      (leadtime |> Pool_common_utils.Time.formatted_timespan)
  | SessionReminder -> "Sessionerinnerung"
  | SessionRegistrationTitle -> "Für diese Session anmelden"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpCTA -> "Jetzt anmelden und an Experimenten teilnehmen."
  | SignUpTitle -> "Anmeldung"
  | SortUngroupedFields -> "Nicht gruppierte Felder sortieren"
  | SwitchChronological -> "Zu chronologische Ansicht wechseln"
  | SwitchGrouped -> "Zu gruppierter Ansicht wechseln"
  | TermsAndConditionsTitle -> "Nutzungsbedingungen"
  | TextTemplates -> "Textelemente"
  | UpcomingSessionsListEmpty ->
    "Sie sind aktuell an keine kommenden Sessions angemeldet."
  | UpcomingSessionsTitle -> "Ihre nächsten Sessions"
  | UserProfileDetailsSubtitle -> "Persönliche Angaben"
  | UserProfileLoginSubtitle -> "Anmeldeinformationen"
  | UserProfilePausedNote ->
    "Sie haben alle Benachrichtigungen für Ihren Benutzer pausiert! (Klicken \
     Sie auf 'Bearbeiten', um diese Einstellung)"
  | Validation -> "Validierung"
  | WaitingListIsDisabled -> "Die Warteliste ist deaktiviert."
;;

let nav_link_to_string = function
  | Admins -> "Administratoren"
  | Assignments -> "Anmeldungen"
  | Contacts -> "Kontakte"
  | CustomFields -> "Felder"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | Filter -> "Filter"
  | Field field -> Locales_de.field_to_string field |> CCString.capitalize_ascii
  | I18n -> "Übersetzungen"
  | Invitations -> "Einladungen"
  | Locations -> "Standorte"
  | LoginInformation -> "Anmeldeinformationen"
  | Login -> "Login"
  | Logout -> "Logout"
  | Mailings -> "Versand"
  | MessageTemplates -> "Nachrichtentemplates"
  | Overview -> "Übersicht"
  | PersonalDetails -> "Persönliche Angaben"
  | Profile -> "Profil"
  | Queue -> "Hintergrundjobs"
  | Rules -> "Regeln"
  | Schedules -> "Prozesse"
  | Sessions -> "Sessions"
  | Settings -> "Einstellungen"
  | Smtp -> "E-Mail Server"
  | SystemSettings -> "Systemeinstellungen"
  | Tenants -> "Tenants"
  | Users -> "Benutzer"
  | WaitingList -> "Warteliste"
;;

let rec hint_to_string = function
  | AllowUninvitedSignup ->
    "Kontakte, die nicht eingeladen wurden, können sich für das Experiment \
     anmelden."
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
  | AssignmentsMarkedAsClosed ->
    "Diese Anmeldungen wurden als gelöscht markiert. Insofern die Kontakte den \
     Experimentkriterien noch entsprechen, können Sie sich erneut an Sessions \
     anmelden."
  | ContactOnWaitingList ->
    "Sie stehen auf der Warteliste. Das Rekrutierungsteam wird Sie einer \
     Session zuweisen."
  | ContactProfileVisibleOverride ->
    "Wenn Sie diese Werte überschreiben werden die Änderungen dem Kontakt \
     angezeigt."
  | CustomFieldAdminInputOnly ->
    Format.asprintf
      "Diese Option schliesst \"%s\" aus."
      (Locales_de.field_to_string Entity_message.Field.Required
       |> CCString.capitalize_ascii)
  | CustomFieldAdminOverride ->
    "Erlaubt Administratoren die vom Kontakt angegebenen Anworten zu \
     überschreiben. Kontakte können die überschriebenen Antworten nicht \
     einsehen."
  | CustomFieldAdminOverrideUpdate ->
    "Wenn Sie diese Option deaktivieren, ignoriert der Filter alle derzeit \
     vorhandenen überschriebenen Antworten."
  | CustomFieldAdminViewOnly ->
    Format.asprintf
      "Diese Option impliziert \"%s\"."
      (Locales_de.field_to_string Entity_message.Field.AdminInputOnly
       |> CCString.capitalize_ascii)
  | CustomFieldContactModel ->
    "Fragen, die Kontakte beantworten können, bzw. müssen. Anhand dieser \
     Informationen werden die Kontakte zu Experimenten eingeladen."
  | CustomFieldExperimentModel -> "Anpassbare Attribute für Experimente."
  | CustomFieldSessionModel -> "Anpassbare Attribute für Sessions."
  | CustomFieldGroups ->
    {|Gruppen, nach denen benutzerdefinierte Felder gruppiert werden können. Das Gruppieren von benutzerdefinierten Feldern hat keine keine Auswirkungen auf ihre Funktionalität. Sie hat lediglich grafische Auswirkungen.|}
  | CustomFieldNoContactValue -> "Durch Kontakt nicht beantwortet"
  | CustomFieldOptionsCompleteness ->
    "Vergewissern Sie sich, dass diese Liste vollständig ist, oder fügen Sie \
     eine Option hinzu, die Sie gewählt werden kann, wenn keine der anderen \
     Optionen zutreffend ist."
  | CustomFieldSort field ->
    Format.asprintf
      "In dieser Reihenfolge werden die %s den Kontakten angezeigt."
      (Locales_de.field_to_string field)
  | CustomFieldTypeText ->
    "Bitte berücksichtigen Sie, dass die Datenqualität bei Texteingaben tiefer \
     ist. Falls die Daten in einer anderen Form erhoben werden können, ist \
     dies zu bevorzugen."
  | CustomFieldTypeSelect ->
    "Nachdem das Feld erstellt wurde, können Sie die verfügbaren Optionen im \
     Abschnitt 'Option' erstellen."
  | CustomFieldTypeMultiSelect -> hint_to_string CustomFieldTypeSelect
  | CustomHtmx s -> s
  | DirectRegistrationDisbled ->
    "Ist diese Option aktiviert, können sich Kontakte auf die Warteliste \
     setzen, aber nicht direkt für das Experiment einschreiben."
  | Distribution ->
    "Mit der Verteilung kann beeinflusst werden, welche Einladungen als erstes \
     versendet werden."
  | EmailPlainText ->
    "Stellen Sie sicher, dass Links als reiner Text angezeigt werden."
  | ExperimentAssignment ->
    "Alle Anmeldungen von Kontakten an Sessions dieses Experiments, sortiert \
     nach Session."
  | ExperimentMailings ->
    {|Einladungsversand dieses Experiments. Die 'Rate' definiert die maximal generierten Einladungen pro Stunde.

    Gestartete Mailings können nicht mehr gelöscht werden.|}
  | ExperimentWaitingList ->
    "Kontakte, die zu diesem Experiment eingeladen wurden, und sich auf die \
     Warteliste gesetzt haben. Sie müssen manuell einer Session zugewiesen \
     werden."
  | ExperimentSessions ->
    {|Alle existierenden Session dieses Experiments.
  Sobald sich jemand angemeldet hat, kann die Session nicht mehr gelöscht werden.
  |}
  | ExperimentSessionsPublic ->
    "Hinweis: Möglicherweise werden einzelne Sessions oder komplette \
     Experimente nicht mehr angezeigt, obwohl im E-Mail aufgeführt. Sobald \
     alle verfügbaren Plätze einer Session belegt sind wird es nichtmehr \
     angezeigt."
  | FilterContacts ->
    "Definieren Sie die Kriterien, anhand welchen Kontakte an dieses \
     Experiment eingeladen werden."
  | I18nText str -> str
  | MissingMessageTemplates (label, languages) ->
    Format.asprintf
      "Das '%s' Template fehlt in den folgenden Sprachen: %s"
      label
      (languages |> CCString.concat ", ")
  | LocationFiles ->
    "Zusatzinformationen zum Standort, wie z.B. eine Wegbeschreibung. \
     Kontakte, die an einer Session an diesem Standort teilnehmen, können auf \
     diese Dateien zugreiffen."
  | LocationSessions ->
    "Zukünftige Sessions, die an diesem Standort durchgeführt werden."
  | Locations ->
    "Standorte, an denen Experimente durchgeführt werden. Jede Session muss \
     eine Location haben."
  | NumberIsSecondsHint -> "Anzahl Sekunden"
  | NumberIsDaysHint -> "Anzahl Tage"
  | NumberIsWeeksHint -> "Anzahl Wochen"
  | Overbook ->
    "Anzahl Kontakte, die sich zusätzlich zur maximalen Anzahl Teilnehmer, an \
     einer Session einschreiben können."
  | Rate -> "Generierte Einladungen pro Stunde"
  | RateDependencyWith ->
    "Zur selben Zeit finden weitere Versande statt, details werden unten \
     angezeigt. Die Summe aller Raten wird automatisch gedrosselt, sobald das \
     maximum des Servers erreicht wird."
  | RateDependencyWithout ->
    "Zur Zeit finden im angegebenen Zeitfenster keine weiteren Versande statt."
  | RateNumberPerMinutes (per_n_minutes, number) ->
    Format.asprintf
      "Generiert alle %d Minuten %.2f neue Einladungen."
      per_n_minutes
      number
  | RegistrationDisabled ->
    "Ist diese Option aktiviert, können sich Kontakte weder anmelden noch auf \
     die Warteliste setzen. Das Experiment ist für die Kontakte nicht \
     ersichtlich."
  | RulesIntro ->
    {|Alle Regeln, welche für Akteure des Tools existieren.
    Beispielsweise, bei der Erstellung eines neuen experiments, werden für dieses Standard Regeln definiert.
    |}
  | ScheduleEvery sec ->
    sec
    |> Pool_common_utils.Time.formatted_timespan
    |> Format.asprintf "alle %s"
  | ScheduleAt time ->
    time
    |> Pool_common_utils.Time.formatted_date_time
    |> Format.asprintf "Am %s"
  | ScheduledIntro ->
    {|Informationen über alle periodischen Hintergrund-Prozesse.

    Beachte: Wenn die Applikation neugestartet wird, werden alle auf "stopped" gesetzt
    |}
  | SearchByFields fields ->
    Format.asprintf
      "Suche nach: %s"
      (fields |> CCList.map Locales_en.field_to_string |> CCString.concat ", ")
  | SessionCancellationWithFollowups ->
    {|Wenn Sie diese Sitzung absagen, werden auch alle Folgesessions abgesagt.

Die folgenden Folgesessions existieren:|}
  | SessionCancellationMessageFollowUps ->
    "Dazugehörige Folgesessions wurden evenfalls abgesagt:"
  | SessionCancelMessage ->
    "Dieser Grund wird allen angemeldeten Kontakten gezeigt."
  | SessionClose ->
    {|NS: Der Kontakt ist nicht an der Session erschienen
    P: Der Kontakt hat am Experiment teilgenommen

    Um 'participated' anzuwählen ist 'show up' erforderlich.
    |}
  | SessionReminderLanguageHint ->
    "Falls sie einen eigenen Erinnerungstext angeben, wählen Sie dessen \
     Sprache hier."
  | SessionRegistrationHint ->
    "Die Registrierung für eine Session ist verbindlich."
  | SessionRegistrationFollowUpHint ->
    "Die Registrierung für eine Session inlk. allen Folgesessions ist \
     verbindlich."
  | SelectedDateIsPast -> "Das gewählte Datum liegt in der Vergangenheit."
  | SignUpForWaitingList ->
    "Das Rekrutierungsteam wird sich mit Ihnen in Verbindung setzen, um Ihnen \
     einen Termin zuzuweisen, wenn ein freier Platz vorhanden ist."
  | SmtpSettingsIntro ->
    {|Die folgende Konfiguration wird vom E-Mail Service verwendet.

    Beachte: Bei Verwendung des Mechanismus für "LOGIN" muss ein Benutzername und Passwort angegeben werden.
    |}
  | TemplateTextElementsHint ->
    "Die folgenden Textbausteine können in den Templates verwendet werden:"
  | TimeSpanPickerHint ->
    "Zeitdauer in Stunden. '1.5' entspricht 1h 30m. '0.75' entspricht 45min."
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelAssignment -> "die Anmeldungen", "annulieren", None
   | CancelAssignmentWithFollowUps ->
     ( "die Anmeldungen"
     , "annulieren"
     , Some "Anmeldungen an Folgesession werden ebenfalls annuliert." )
   | CancelSession -> "die Session", "absagen", None
   | DeleteCustomField -> "das Feld", "löschen", None
   | DeleteCustomFieldOption -> "das Option", "löschen", None
   | DeleteEmailSuffix -> "das Suffix", "löschen", None
   | DeleteExperiment -> "das Experiment", "löschen", None
   | DeleteExperimentFilter -> "den Filter", "löschen", None
   | DeleteFile -> "die Datei", "löschen", None
   | DeleteMailing -> "den Versand", "löschen", None
   | DeleteSession -> "die Session", "löschen", None
   | MarkAssignmentAsDeleted -> "die Anmeldung", "als gelöscht markieren", None
   | MarkAssignmentWithFollowUpsAsDeleted ->
     ( "die Anmeldung"
     , "als gelöscht markieren"
     , Some
         "Anmeldungen an Folgesession werden ebenfalls als gelöscht markiert." )
   | PublisCustomField ->
     ( "das Feld und alle dazugehörigen Optionen"
     , "publizieren"
     , Some "Sie werden das Feld nicht mehr löschen können." )
   | PublisCustomFieldOption ->
     ( "die Option"
     , "publizieren"
     , Some "Sie werden die Option nicht mehr löschen können." )
   | RemoveRule -> "die Regel", "löschen", None
   | RevokeRole -> "die Rolle", "entfernen", None
   | StopMailing -> "den Versand", "stoppen", None)
  |> fun (obj, action, additive) ->
  Format.asprintf "Sind Sie sicher, dass Sie %s %s wollen?" obj action
  |> fun msg ->
  additive
  |> CCOption.map_or ~default:msg (fun additive ->
       Format.asprintf "%s %s" msg additive)
;;
