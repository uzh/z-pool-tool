open Entity_i18n

let to_string = function
  | Activity -> "Aktivität"
  | Address -> "Addresse"
  | AdminComment -> "Administrator Kommentar"
  | AssignmentEditTagsWarning ->
    "Bitte beachten Sie, dass durch die Bearbeitung der Anmeldung keine Tags \
     zugewiesen oder entfernt werden, die durch die Teilnahme an dieser \
     Session dem Kontakt zugewiesen wurden. Wenn dies erforderlich ist, wenden \
     Sie sich bitte an eine Person mit den erforderlichen Berechtigungen."
  | AssignmentListEmpty -> "Es existieren keine Anmeldungen für diese Session."
  | AvailableSpots -> "Freie Plätze"
  | Canceled -> "Abgesagt"
  | Closed -> "Geschlossen"
  | ContactWaitingListEmpty -> "Sie sind aktuell auf keiner Warteliste."
  | ContactWaitingListTitle -> "Auf der Warteliste"
  | DashboardProfileCompletionText ->
    "Ihr Profil ist unvollständig. Um zu mehr Experimenten eingeladen zu \
     werden, vervollständigen Sie Ihr Profil."
  | DashboardProfileCompletionTitle -> "Profilvervollständigung"
  | DashboardTitle -> "Dashboard"
  | DeletedAssignments -> "Gelöschte Anmeldungen"
  | Disabled ->
    Locales_de.field_to_string Entity_message_field.Disabled
    |> CCString.capitalize_ascii
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
  | EmptyListGeneric -> "Es konnten keine Einträge gefunden werden."
  | EnrollInExperiment -> "Zum Experiment anmelden"
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
  | ImportConfirmationNote ->
    "Bitte geben Sie ein neues Paswort an. Ihre restlichen Angaben wurden \
     automatisch übernommen."
  | ImportConfirmationTitle -> "Neues Passwort"
  | ImportPendingNote ->
    "Der Import Ihres Users ist noch nicht abgeschlossen. Bitte kontrollieren \
     Sie Ihren Posteingang oder kontaktieren Sie einen Administrator."
  | ImportPendingTitle -> "Pendenter Import"
  | InvitationsStatistics -> "Einladungsstatistik"
  | InvitationsStatisticsIntro ->
    "Diese Tabelle zeigt, wie oft die Kontakte die Einladung zu diesem \
     Experiment erhalten haben."
  | LocationDetails -> "Standortdetails"
  | NoEntries field ->
    Format.asprintf
      "Es existiert noch keine %s."
      (Locales_de.field_to_string field)
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
  | ExperimentMessagingSubtitle -> "Nachrichtenversand"
  | RateTotalSent number ->
    Format.asprintf "Total generierter Einladungen: %d" number
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | Reminder -> "Erinnerung"
  | ResendReminders -> "Erinnerungen erneut schicken"
  | RoleApplicableToAssign -> "Zuweisbare Benutzer"
  | RoleCurrentlyAssigned -> "Aktuell zugewiesen"
  | RoleCurrentlyNoneAssigned field ->
    Format.asprintf
      "Aktuell sind keine %s zugewiesen."
      (Locales_de.field_to_string field)
  | RolesGranted -> "Zugewiesene Rollen"
  | SentInvitations -> "Versendete Einladungen"
  | SelectedTags -> "Aktuell zugewiesene Tags"
  | SelectedTagsEmpty -> "Keine Tags zugewiesen"
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
  | SwapSessionsListEmpty ->
    "Es wurden keine Sessions gefunden, der Sie diesen Kontakt zuweisen können."
  | SwitchChronological -> "Zu chronologische Ansicht wechseln"
  | SwitchGrouped -> "Zu gruppierter Ansicht wechseln"
  | TermsAndConditionsLastUpdated ptime ->
    Format.asprintf
      "Zuletzt angepasst: %s"
      (Pool_common_utils.Time.formatted_date ptime)
  | TermsAndConditionsTitle -> "Nutzungsbedingungen"
  | TermsAndConditionsUpdated ->
    "Wir haben kürzlich unsere Allgemeinen Geschäftsbedingungen geändert. \
     Bitte lesen und akzeptieren Sie diese, um fortzufahren."
  | TextTemplates -> "Textelemente"
  | UpcomingSessionsListEmpty ->
    "Sie sind aktuell an keine kommenden Sessions angemeldet."
  | PastExperimentListPublicTitle -> "Teilgenommene Experimente"
  | PastSessionsTitle -> "Ihre vergangenen Sessions"
  | PoolStatistics -> "Pool-Statistik"
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
  | ContactInformation -> "Kontaktangaben"
  | Contacts -> "Kontakte"
  | Credits -> "Impressum"
  | CustomFields -> "Felder"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | ExternalDataIds -> "Externe Daten Identifikatoren"
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
  | OrganisationalUnits -> "Organisationseinheiten"
  | Overview -> "Übersicht"
  | ParticipationTags -> "Teilnahmetags"
  | PersonalDetails -> "Persönliche Angaben"
  | PrivacyPolicy -> "Datenschutzerklärung"
  | Profile -> "Profil"
  | Queue -> "Hintergrundjobs"
  | Rules -> "Regeln"
  | Schedules -> "Prozesse"
  | Sessions -> "Sessions"
  | Settings -> "Einstellungen"
  | Smtp -> "E-Mail Server"
  | SystemSettings -> "Systemeinstellungen"
  | Tags -> "Tags"
  | Tenants -> "Tenants"
  | Users -> "Benutzer"
  | WaitingList -> "Warteliste"
;;

let rec hint_to_string = function
  | AdminOverwriteContactValues ->
    {|Wenn Sie einen der folgenden Werte anpassen, ist dies für den Kontakt nicht ersichtlich.

Wird nach diesem Feld gefiltert, wird der überschreibende Wert bevorzugt.
|}
  | AllowUninvitedSignup ->
    "Alle Kontakte (eingeladen oder nicht), können sich für das Experiment \
     anmelden."
  | AssignmentConfirmationMessageFollowUps ->
    "Sie wurden außerdem den folgenden Folgesitzungen zugewiesen:"
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
  | AssignmentsMarkedAsClosed ->
    "Diese Anmeldungen wurden als gelöscht markiert. Insofern die Kontakte den \
     Experimentkriterien noch entsprechen, können Sie sich erneut an Sessions \
     anmelden."
  | ContactCurrentCellPhone cell_phone ->
    Format.asprintf "Ihre aktuelle Mobiltelefonnummer lautet %s." cell_phone
  | ContactDoesNotMatchFilter ->
    "Der Kontakt erfüllt nicht die im Filter bestimmten Kriterien für dieses \
     Experiment, kann jedoch trotzdem angemeldet werden."
  | ContactNoCellPhone -> "Sie haben noch keine Mobiltelefonnummer verifiziert."
  | ContactEnterCellPhoneToken cell_phone ->
    Format.asprintf
      "PBitte geben Sie den Verifizierungscode ein, den wir Ihnen an %s \
       geschickt haben. Der Code ist eine Stunde lang gültig."
      cell_phone
  | ContactPhoneNumberVerificationWasReset ->
    "Sie können nun eine neue Telefonnummer eingeben."
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
  | CustomFieldAnsweredOnRegistration ->
    "Dieses Feld wurde vom Kontakt bereits bei der Registrierung beantwortet \
     und kann vom Kontakt selbst nicht mehr verändert werden."
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
  | CustomFieldPromptOnRegistration ->
    "Ist diese Option aktiviert, wird dieses Feld bereits bei der \
     Registrierung abgefragt, jedoch dem Kontakt nicht mehr im Benutzerprofil \
     angezeigt."
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
    {|Die Verwendung von E-Mails im Klartext als Ausweichlösung gewährleistet eine universelle Lesbarkeit und Barrierefreiheit. Sie können den Rich-Text von oben kopieren, indem Sie die Schaltfläche in der oberen rechten Ecke dieses Textfeldes verwenden.
  Achten Sie darauf, Links und URLs als reinen Text anzuzeigen.|}
  | ExperimentAssignment ->
    "Alle Anmeldungen von Kontakten an Sessions dieses Experiments, sortiert \
     nach Session."
  | ExperimentContactPerson ->
    "Die E-Mail-Adresse des ausgewählten Nutzers wird als 'reply-to' Adresse \
     für alle experimentbezogenen E-Mails verwendet."
  | ExperimentMailings ->
    {|Einladungsversand dieses Experiments.

    Die Limite definiert die Anzahl an Einladungen für dieses Mailing und die Anzahl Einladungen zeigt den momentanen Stand der versendeten/bearbeiteten.
    Falls mehrere Mailings parallel laufen, kann es sein, dass die Anzahl runtergesetzt wird und dadurch nicht das gewünschte Limit erreicht.

    Gestartete Mailings können nicht mehr gelöscht werden.|}
  | ExperimentMailingsRegistrationDisabled ->
    {|Die Registrierung für dieses Experiment ist derzeit deaktiviert. Einladungen werden weiterhin verschickt, wenn ein Mailing erstellt wird, aber die Kontakte können sich nicht für eine Session anmelden.|}
  | ResendRemindersChannel ->
    "Wenn Sie sich dafür entscheiden, die Erinnerungen als Textnachrichten zu \
     versenden, erhalten Kontakte, die keine verifizierte Handynummer haben, \
     die Erinnerung per E-Mail."
  | ResendRemindersWarning ->
    {sql|Es wurden noch keine automatischen Erinnerungen für diese Session verschickt. Stellen Sie sicher, dass das Nachrichtentemplate korrekt ist, falls Sie die Erinnerungen jetzt auslösen wollen.

Wenn Sie die Erinnerungen jetzt manuell auslösen werden über den gewählten Nachrichtenkanal keine automatischen Erinnerungen mehr verschickt.|sql}
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
  | ExternalDataRequired ->
    "Pro Anmeldung ist ein Identifikator für externe Daten obligatorisch \
     (spätestens wenn eine Session abgeschlossen wird)."
  | FilterContacts ->
    "Definieren Sie die Kriterien, anhand welchen Kontakte an dieses \
     Experiment eingeladen werden."
  | TestPhoneNumber ->
    "Bitte geben Sie eine Telefonnummer an, an die wir eine einzige \
     Testnachricht schicken können, um den API Key zu verifizieren. Die Nummer \
     muss im Format +41791234567 sein."
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
  | MailingLimit -> "Max. generierte Einladungen pro Mailing."
  | NumberIsSecondsHint -> "Anzahl Sekunden"
  | NumberIsDaysHint -> "Anzahl Tage"
  | NumberIsWeeksHint -> "Anzahl Wochen"
  | Overbook ->
    "Anzahl Kontakte, die sich zusätzlich zur maximalen Anzahl Teilnehmer, an \
     einer Session einschreiben können."
  | PartialUpdate ->
    "Das folgende Formular wird die geänderten Werte sofort speichern. Sie \
     brauchen das Formular nicht abzuschicken."
  | ParticipationTags ->
    "Tags, welche den Teilnehmern nach einer Teilnahme an einer Session dieses \
     Experiments automatisch zugewiesen werden."
  | PauseAccountAdmin ->
    "Solange das Konto pausiert ist, wird der Kontakt zu keinen weiteren \
     Experimenten eingeladen."
  | PauseAccountContact ->
    "Solange Ihr Konto pausiert ist, werden Sie nicht zu weiteren Experimenten \
     eingeladen."
  | PromoteContact ->
    "Achtung: einmalige Aktion. Der Kontakt wird zu einem Admin befördert, \
     dieser wird anschliessend nicht mehr für Experimente eingeladen und kann \
     sich nicht mehr für solche registrieren."
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
  | ResetInvitations ->
    "Einladungen zurücksetzen, alle bisherigen Einladungen werden für \
     zukünftige Versande ignoriert."
  | ResetInvitationsLastReset reset_at ->
    Format.asprintf
      "Die Einladungen wirden zuletzt am <strong>%s</strong> zurückgesetzt."
      (Utils_time.formatted_date_time reset_at)
  | RoleIntro (singular, plural) ->
    Format.asprintf
      "Wenn kein %s angegeben wird, gilt die Rolle für alle %s."
      (Locales_en.field_to_string singular)
      (Locales_en.field_to_string plural)
  | RulesIntro ->
    {|Alle Regeln, welche für Akteure des Tools existieren.
    Beispielsweise, wenn ein neues Experiment erstellt wird, wird für dieses ein Standard Regelsatz hinzugefügt.
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
  | SessionCloseParticipationTagsSelected ->
    "Die folgenden Tags werden allen Teilnehmer/innen zugewiesen, die an \
     diesem Experiment teilgenommen haben:"
  | SessionCloseNoParticipationTagsSelected ->
    "Es wurden keine Tags ausgewählt, die den Teilnehmer/innen zugewiesen \
     werden, die an diesem Experiment teilgenommen haben."
  | SessionCloseHints ->
    Format.asprintf
      {|<strong>%s</strong> und <strong>%s</strong> schliessen sich gegenseitig aus.<br>
Wenn keine der Checkboxen angewählt ist, bedeutet das, dass der Kontakt erschienen ist, aber nicht teilgenommen hat.|}
      (Locales_de.field_to_string Entity_message_field.NoShow)
      (Locales_de.field_to_string Entity_message_field.Participated)
  | SessionCloseLegend ->
    {|NS: Der Kontakt ist nicht an der Session erschienen
    P: Der Kontakt hat am Experiment teilgenommen|}
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
  | SmtpSettingsDefaultFlag ->
    "Achtung: Ist eine andere SMTP Konfiguration als Standard markiert, wird \
     diese Einstellung angepasst. Nur eine Konfiguration kann als Standard \
     markiert sein."
  | SmtpSettingsIntro ->
    {|Die folgende Konfiguration wird vom E-Mail Service verwendet.

    Beachte: Bei Verwendung des Mechanismus für "LOGIN" muss ein Benutzername und Passwort angegeben werden.
    |}
  | SwapSessions ->
    {|Das Ändern der Session wird nur diese Anmeldung anpassen. Wenn es \
     Folgezuweisungen gibt, müssen diese manuell aktualisiert werden.

Es können nur Sitzungen mit freien Plätzen ausgewählt werden.|}
  | TagsIntro ->
    "Die definierten Tags können an die verschiedenen Objekte (z.B. Kontakte) \
     angehängt werden. Diese Tags können im Experiment-Filter verwendet werden \
     um sie ein- oder auszuschliessen."
  | TemplateTextElementsHint ->
    "Die folgenden Textbausteine können in den Templates verwendet werden. \
     Klicken Sie auf die Labels, um sie in die Zwischenablage zu kopieren."
  | TimeSpanPickerHint -> "Zeitdauer in Minuten."
  | WaitingListPhoneMissingContact ->
    "Sie haben in Ihrem Profil noch keine Telefonnummer angegenen. Wir bitten \
     Sie, eine Telefonnummer anzugeben, damit das Rekrutierungsteam Sie \
     kontaktieren kann."
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelAssignment -> "die Anmeldungen", "annulieren", None
   | CancelAssignmentWithFollowUps ->
     ( "die Anmeldungen"
     , "annulieren"
     , Some "Anmeldungen an Folgesession werden ebenfalls annuliert." )
   | CancelSession -> "die Session", "absagen", None
   | CloseSession ->
     ( "die Session"
     , "schliessen"
     , Some "Diese Aktion kann nicht rückgängig gemacht werden." )
   | DeleteCustomField -> "das Feld", "löschen", None
   | DeleteCustomFieldOption -> "das Option", "löschen", None
   | DeleteEmailSuffix -> "das Suffix", "löschen", None
   | DeleteExperiment -> "das Experiment", "löschen", None
   | DeleteExperimentFilter -> "den Filter", "löschen", None
   | DeleteFile -> "die Datei", "löschen", None
   | DeleteMailing -> "den Versand", "löschen", None
   | DeleteMessageTemplate -> "das Nachrichtentemplate", "löschen", None
   | DeleteSession -> "die Session", "löschen", None
   | MarkAssignmentAsDeleted -> "die Anmeldung", "als gelöscht markieren", None
   | MarkAssignmentWithFollowUpsAsDeleted ->
     ( "die Anmeldung"
     , "als gelöscht markieren"
     , Some
         "Anmeldungen an Folgesession werden ebenfalls als gelöscht markiert." )
   | PauseAccount -> "den Account", "pausieren", None
   | PromoteContact ->
     ( "den Kontakt"
     , "befördern"
     , Some
         "Dieser wird nicht mehr für Experimente eingeladen und kann sich \
          nicht mehr für solche registrieren." )
   | PublisCustomField ->
     ( "das Feld und alle dazugehörigen Optionen"
     , "publizieren"
     , Some "Sie werden das Feld nicht mehr löschen können." )
   | PublisCustomFieldOption ->
     ( "die Option"
     , "publizieren"
     , Some "Sie werden die Option nicht mehr löschen können." )
   | ReactivateAccount -> "den Account", "reaktivieren", None
   | RemoveRule -> "die Regel", "löschen", None
   | RemoveTag -> "den Tag", "entfernen", None
   | ResetInvitations ->
     ( "die Einladungen"
     , "zurücksetzen"
     , Some
         "Anschliessend werden alle bisherigen Einladungen bis zum jetztigen \
          Zeitpunkt ignoriert, heisst bereits zuvor Eingeladene Kontakte \
          erhalten erneut eine Einladung." )
   | RevokeRole -> "die Rolle", "entfernen", None
   | StopMailing -> "den Versand", "stoppen", None)
  |> fun (obj, action, additive) ->
  Format.asprintf "Sind Sie sicher, dass Sie %s %s wollen?" obj action
  |> fun msg ->
  additive
  |> CCOption.map_or ~default:msg (fun additive ->
    Format.asprintf "%s %s" msg additive)
;;
