open Entity_i18n

let error_to_string = Locales_de.error_to_string

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
  | CustomFieldsSettings ->
    "In der folgenden Liste können Sie bestimmen, in welcher Tabelle \
     zusätzlich zu den Kontaktangaben auch die individuellen Angaben angezeigt \
     werden sollen."
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
  | EmailConfirmationTitle -> "Bestätigung Ihrer E-Mail-Adresse"
  | EmptyListGeneric -> "Es konnten keine Einträge gefunden werden."
  | EmtpyList field ->
    Format.asprintf
      "Es sind keine %s vorhanden."
      (Locales_de.field_to_string field)
  | EnrollInExperiment -> "Zum Experiment anmelden"
  | ExperimentListEmpty ->
    "Aktuell gibt es keine Experimente, an denen Sie teilnehmen können."
  | ExperimentListPublicTitle -> "Neuanmeldung zu Experiment-Sessions"
  | ExperimentListTitle -> "Experimente"
  | ExperimentMessagingSubtitle -> "Nachrichtenversand"
  | ExperimentNewTitle -> "Neues Experiment erstellen"
  | ExperimentSessionReminderHint ->
    "Dies sind Standardeinstellungen für die Sessions dieses Experiment. Diese \
     Einstellungen können pro Session angepasst werden."
  | ExperimentStatistics -> "Experimentstatistik"
  | ExperimentWaitingListTitle -> "Warteliste"
  | Files -> "Dateien"
  | FilterContactsDescription ->
    {|<p>Um Kontakte zu diesem Experiment einzuladen, folgen Sie diesen Schritten:</p>
    <ol>
      <li>Erstellen Sie einen Filter mit einer oder mehreren Bedingungen, um festzulegen, welche Kontakte Sie in dieses Experiment aufnehmen möchten.</li>
      <li>Erstellen Sie die Sessions, in denen Sie das Experiment durchführen möchten.</li>
      <li>Erstellen Sie ein oder mehrere Mailings, um den Versand von E-Mails an diese Teilnehmer zu starten.</li>
    </ol>|}
  | FilterNrOfContacts ->
    "Anzahl der Kontakte, die den Kriterien dieses Filters entsprechen:"
  | FilterNrOfSentInvitations -> "Anzahl bereits eingeladener Kontakte:"
  | FollowUpSessionFor -> "Folgesession für:"
  | ImportConfirmationNote ->
    "Bitte geben Sie ein neues Paswort an. Ihre restlichen Angaben wurden \
     automatisch übernommen."
  | ImportConfirmationTitle -> "Neues Passwort"
  | ImportPendingNote ->
    "Der Import Ihres Users ist noch nicht abgeschlossen. Bitte kontrollieren \
     Sie Ihren Posteingang oder kontaktieren Sie einen Administrator."
  | ImportPendingTitle -> "Pendenter Import"
  | IncompleteSessions -> "Nicht abgeschlossene Sessions"
  | InvitationsStatistics -> "Einladungsstatistik"
  | InvitationsStatisticsIntro ->
    "Diese Tabelle zeigt, wie oft die Kontakte die Einladung zu diesem \
     Experiment erhalten haben."
  | JobCloneOf -> "Dieser Hintergrundjob ist eine Kopie von"
  | LocationDetails -> "Standortdetails"
  | LocationFileNew -> "Neue Datei zu Standort hinzufügen"
  | LocationListTitle -> "Standorte"
  | LocationNewTitle -> "Neuer Standort erstellen"
  | LocationNoFiles -> "Es existieren keine Dateien zu diesem Standort."
  | LocationNoSessions -> "Keine Sessions für diesen Standort gefunden."
  | LocationStatistics -> "Standortstatistik"
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Versand vom %s" (Utils_time.formatted_date_time start)
  | MailingDistributionDescription ->
    {|<ol>
      <li>Wählen Sie aus, nach welchem Feld und in welcher Reihenfolge Sie die Kontakte sortieren möchten.</li>
      <li>Drücken Sie die Schaltfläche "Hinzufügen", um den Sortierparameter hinzuzufügen.</li>
      <li>Wiederholen Sie diesen Vorgang, um weitere Parameter hinzuzufügen. Sie können sie durch "drag and drop" sortieren.</li>
    </ol>|}
  | MailingExperimentSessionFullyBooked ->
    "Alle Sessions sind ausgebucht. Es werden keine Einladungen versendet \
     (unabhängig ob z.Z. Mailings aktiv sind).\n\n\
     Füge zusätzliche Sessions zum Experiment hinzu."
  | MailingNewTitle -> "Neuen Versand erstellen"
  | MessageHistory name -> Format.asprintf "Nachrichtenverlauf von %s" name
  | NoEntries field ->
    Format.asprintf
      "Es existiert noch keine %s."
      (Locales_de.field_to_string field)
  | Note -> "Hinweis"
  | OurPartners -> "Unsere Partner"
  | Past -> "Vergangen"
  | PastExperimentListPublicTitle -> "Teilgenommene Experimente"
  | PastSessionsTitle -> "Ihre vergangenen Sessions"
  | PoolStatistics -> "Pool-Statistik"
  | PoolOptOut -> "Ich möchte diese Emails in Zukunft nicht mehr erhalten."
  | ProfileCompletionText ->
    {|Die folgenden Angaben werden benötigt, um an Experimente eingeladen werden zu können. Weitere Angaben können anschliessend in Ihrem Profil gemacht werden.

Sie kommen für mehr Experimente in Frage, umso kompletter Ihr Profil ist.|}
  | Reminder -> "Erinnerung"
  | ResendReminders -> "Erinnerungen erneut schicken"
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | RoleApplicableToAssign -> "Zuweisbare Benutzer"
  | RoleCurrentlyAssigned -> "Aktuell zugewiesen"
  | RoleCurrentlyNoneAssigned field ->
    Format.asprintf
      "Aktuell sind keine %s zugewiesen."
      (Locales_de.field_to_string field)
  | RolesGranted -> "Zugewiesene Rollen"
  | SelectedTags -> "Aktuell zugewiesene Tags"
  | SelectedTagsEmpty -> "Keine Tags zugewiesen"
  | SentInvitations -> "Versendete Einladungen"
  | SessionCloseScreen -> "Bildschirm zum Beenden der Sessions"
  | SessionDetailTitle start ->
    Format.asprintf "Session am %s" (Utils_time.formatted_date_time start)
  | SessionIndent -> "Einrückungen groupieren Folgesessions."
  | SessionRegistrationTitle -> "Für diese Session anmelden"
  | SessionReminder -> "Sessionerinnerung"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpTitle -> "Anmeldung"
  | SortUngroupedFields -> "Nicht gruppierte Felder sortieren"
  | SwapSessionsListEmpty ->
    "Es wurden keine Sessions gefunden, der Sie diesen Kontakt zuweisen können."
  | SwitchChronological -> "Zu chronologische Ansicht wechseln"
  | SwitchGrouped -> "Zu gruppierter Ansicht wechseln"
  | System -> "System"
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
  | ActorPermissions -> "Persönliche Berechtigungen"
  | Admins -> "Administratoren"
  | Assignments -> "Anmeldungen"
  | ContactInformation -> "Kontaktangaben"
  | Contacts -> "Kontakte"
  | Credits -> "Impressum"
  | CustomFields -> "Felder"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | ExternalDataIds -> "Externe Daten Identifikatoren"
  | Field field -> Locales_de.field_to_string field |> CCString.capitalize_ascii
  | Filter -> "Filter"
  | I18n -> "Texte"
  | Invitations -> "Einladungen"
  | Locations -> "Standorte"
  | Login -> "Login"
  | LoginInformation -> "Anmeldeinformationen"
  | Logout -> "Logout"
  | Mailings -> "Versand"
  | MessageHistory -> "Nachrichtenverlauf"
  | MessageTemplates -> "Nachrichtentemplates"
  | OrganisationalUnits -> "Organisationseinheiten"
  | Overview -> "Übersicht"
  | ParticipationTags -> "Teilnahmetags"
  | PersonalDetails -> "Persönliche Angaben"
  | PrivacyPolicy -> "Datenschutzerklärung"
  | Profile -> "Profil"
  | Queue -> "Hintergrundjobs"
  | RolePermissions -> "Rollenberechtigungen"
  | Schedules -> "Prozesse"
  | Sessions -> "Sessions"
  | Settings -> "Einstellungen"
  | Smtp -> "E-Mail Server (SMTP)"
  | SystemSettings -> "Systemeinstellungen"
  | Tags -> "Tags"
  | Tenants -> "Tenants"
  | TextMessages -> "SMS"
  | Users -> "Benutzer"
  | WaitingList -> "Warteliste"
;;

let rec hint_to_string = function
  | AdminOverwriteContactValues ->
    {|Wenn Sie einen der folgenden Werte überschreiben, ist dies für den Kontakt nicht ersichtlich. Wenn ein vom Kontakt eingegebener Wert überschrieben wird, wird der überschriebene Wert unterhalb des Eingabefeldes angezeigt.

Beim Einladen von Kontakten bevorzugt der Filter den überschreibenden Wert, wenn beide vorhanden sind.|}
  | AllowUninvitedSignup ->
    "Alle Kontakte (eingeladen oder nicht), können sich für das Experiment \
     anmelden."
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
  | AssignmentCancellationMessageFollowUps ->
    "Die Anmeldung zu folgenden Sessions wurde ebenfalls annulliert:"
  | AssignmentConfirmationMessageFollowUps ->
    "Sie wurden außerdem den folgenden Folgesitzungen zugewiesen:"
  | AssignmentsMarkedAsClosed ->
    "Diese Anmeldungen wurden als gelöscht markiert. Insofern die Kontakte den \
     Experimentkriterien noch entsprechen, können Sie sich erneut an Sessions \
     anmelden."
  | ContactCurrentCellPhone cell_phone ->
    Format.asprintf "Ihre aktuelle Mobiltelefonnummer lautet %s." cell_phone
  | ContactEnrollmentDoesNotMatchFilter ->
    "Der Kontakt erfüllt nicht die im Filter bestimmten Kriterien für dieses \
     Experiment, kann jedoch trotzdem angemeldet werden."
  | ContactEnrollmentRegistrationDisabled ->
    "Die Registrierung für dieses Experiment ist derzeit deaktiviert. Kontakte \
     können sich nicht selbst für dieses Experiment einschreiben."
  | ContactEnterCellPhoneToken cell_phone ->
    Format.asprintf
      "Bitte geben Sie den Verifizierungscode ein, den wir Ihnen an %s   \
       geschickt haben. Der Code ist eine Stunde lang gültig."
      cell_phone
  | ContactLanguage ->
    "Bei einigen Experimenten wird in einer anderen Sprache kommuniziert, ohne \
     Rücksicht auf die Kontaktsprache."
  | ContactNoCellPhone -> "Sie haben noch keine Mobiltelefonnummer verifiziert."
  | ContactOnWaitingList ->
    "Sie stehen auf der Warteliste. Das Rekrutierungsteam wird Sie einer \
     Session zuweisen."
  | ContactPhoneNumberVerificationWasReset ->
    "Sie können nun eine neue Telefonnummer eingeben."
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
  | CustomFieldSessionModel -> "Anpassbare Attribute für Sessions."
  | CustomFieldSort field ->
    Format.asprintf
      "In dieser Reihenfolge werden die %s den Kontakten angezeigt."
      (Locales_de.field_to_string field)
  | CustomFieldTypeMultiSelect -> hint_to_string CustomFieldTypeSelect
  | CustomFieldTypeSelect ->
    "Nachdem das Feld erstellt wurde, können Sie die verfügbaren Optionen im \
     Abschnitt 'Option' erstellen."
  | CustomFieldTypeText ->
    "Bitte berücksichtigen Sie, dass die Datenqualität bei Texteingaben tiefer \
     ist. Falls die Daten in einer anderen Form erhoben werden können, ist \
     dies zu bevorzugen."
  | CustomHtmx s -> s
  | DefaultReminderLeadTime lead_time ->
    Format.asprintf
      "Bleibt diese Angabe leer, wird die Standardvorlaufzeit von %s verwendet."
      (lead_time |> Utils_time.formatted_timespan)
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
  | ExperimentLanguage ->
    "ist eine Experimentsprache definiert, werden alle Nachrichten, die dieses \
     Experiment betreffen, in dieser Sprache gesendet, ohne Rücksicht auf die \
     Kontaktsprache."
  | ExperimentMailings ->
    {|Einladungsversand dieses Experiments.

    Die Limite definiert die Anzahl an Einladungen für dieses Mailing und die Anzahl Einladungen zeigt den momentanen Stand der versendeten/bearbeiteten.
    Falls mehrere Mailings parallel laufen, kann es sein, dass die Anzahl runtergesetzt wird und dadurch nicht das gewünschte Limit erreicht.

    Gestartete Mailings können nicht mehr gelöscht werden.|}
  | ExperimentMailingsRegistrationDisabled ->
    {|Die Registrierung für dieses Experiment ist derzeit deaktiviert. Einladungen werden weiterhin verschickt, wenn ein Mailing erstellt wird, aber die Kontakte können sich nicht für eine Session anmelden.|}
  | ExperimentMessageTemplates ->
    {|Nachrichten bezüglich dieses Experiments oder Session, die an Kontakte gesendet werden, können angepasst werden, wenn Sie Informationen hinzufügen oder entfernen möchten. Die Vorlage wird in der folgenden Hierarchie ausgewählt: Session-spezifisch > Experiment-spezifisch > Standard

Wenn eine Experimentsprache angegeben ist, werden alle Nachrichten in dieser Sprache gesendet. Wenn keine experimentenspezifische Sprache definiert ist, werden die Nachrichten in der Anzeigesprache des Kontakts gesendet.|}
  | ExperimentSessions ->
    {|Alle existierenden Session dieses Experiments.
  Sobald sich jemand angemeldet hat, kann die Session nicht mehr gelöscht werden.|}
  | ExperimentSessionsPublic ->
    "Hinweis: Möglicherweise werden einzelne Sessions oder komplette \
     Experimente nicht mehr angezeigt, obwohl im E-Mail aufgeführt. Sobald \
     alle verfügbaren Plätze einer Session belegt sind, wird sie nicht mehr \
     angezeigt."
  | ExperimentStatisticsRegistrationPossible ->
    "Dies gilt als richtig, wenn die Registrierung nicht deaktiviert ist und \
     es zukünftige Sessions mit freien Plätzen gibt."
  | ExperimentStatisticsSendingInvitations ->
    {|Sending: Derzeit läuft ein Mailing.
  
Scheduled: Es läuft kein Mailing, aber zukünftige Mailings sind geplant|}
  | ExperimentWaitingList ->
    "Kontakte, die zu diesem Experiment eingeladen wurden, und sich auf die \
     Warteliste gesetzt haben. Sie müssen manuell einer Session zugewiesen \
     werden."
  | ExternalDataRequired ->
    "Pro Anmeldung ist ein Identifikator für externe Daten obligatorisch \
     (spätestens wenn eine Session abgeschlossen wird)."
  | GtxKeyMissing ->
    "Es wurde kein GTX Api Key hinterlegt, weshalb keine Textnachrichten \
     verschickt werden."
  | GtxKeyStored ->
    "Ein GTX Api-Schlüssel ist hinterlegt. Textnachrichten werden verschickt."
  | I18nText str -> str
  | LocationFiles ->
    "Zusatzinformationen zum Standort, wie z.B. eine Wegbeschreibung. \
     Kontakte, die an einer Session an diesem Standort teilnehmen, können auf \
     diese Dateien zugreiffen."
  | LocationsIndex ->
    "Standorte, an denen Experimente durchgeführt werden. Jede Session muss \
     eine Location haben."
  | MailingLimit -> "Max. generierte Einladungen pro Mailing."
  | MessageTemplateAccountSuspensionNotification ->
    "Diese Nachricht wird an einen Benutzer gesendet, nachdem sein Konto wegen \
     zu vieler fehlgeschlagener Anmeldeversuche vorübergehend gesperrt wurde."
  | MessageTemplateAssignmentCancellation ->
    "Diese Nachricht wird verwendet, um Kontakte über die Annullierung einer \
     Sessionanmeldung zu informieren."
  | MessageTemplateAssignmentConfirmation ->
    "Diese Nachricht wird an Kontakte gesendet, nachdem sie sich erfolgreich \
     für eine Sitzung angemeldet haben."
  | MessageTemplateAssignmentSessionChange ->
    "Diese Nachricht wird an Kontakte gesendet, nachdem sie von einem \
     Administrator einer anderen Session zugewiesen wurden."
  | MessageTemplateContactEmailChangeAttempt ->
    "Diese Nachricht wird an einen Benutzer gesendet, nachdem jemand versucht \
     hat, seine E-Mail-Adresse in eine bestehende Adresse zu ändern."
  | MessageTemplateContactRegistrationAttempt ->
    "Diese Nachricht wird an einen Benutzer gesendet, nachdem er versucht hat, \
     sich mit einer bestehenden E-Mail-Adresse zu registrieren."
  | MessageTemplateEmailVerification ->
    "Diese E-Mail wird verwendet, um neue E-Mail-Adressen nach der Änderung \
     einer Konto-E-Mail-Adresse zu verifizieren. Sie können die \
     SMS-Texteingabe ignorieren."
  | MessageTemplateExperimentInvitation ->
    "Diese Nachricht wird gesendet, um Kontakte zu Experimenten einzuladen."
  | MessageTemplatePasswordChange ->
    "Diese Nachricht wird gesendet, um Benutzer zu benachrichtigen, dass das \
     Kontopasswort geändert wurde."
  | MessageTemplatePasswordReset ->
    "Diese Nachricht sendet das Passwort-Reset-Token an die angegebene Adresse."
  | MessageTemplatePhoneVerification ->
    "Diese Nachricht sendet das Token zur Überprüfung der Telefonnummer an das \
     Telefon des Kontakts. Sie können die E-Mail und den Klartext ignorieren."
  | MessageTemplateProfileUpdateTrigger ->
    "Diese Nachricht wird verwendet, um Kontakte zu benachrichtigen, die ihr \
     Profil vor einiger Zeit zuletzt aktualisiert haben, und sie aufzufordern, \
     ihre persönlichen Daten zu kontrollieren."
  | MessageTemplateSessionCancellation ->
    "Diese Nachricht wird verwendet, um Kontakte über die Annullierung einer \
     Session zu informieren."
  | MessageTemplateSessionReminder ->
    "Diese Nachricht erinnert Kontakte an bevorstehende Sessions, für die sie \
     sich angemeldet haben."
  | MessageTemplateSessionReschedule ->
    "Diese Nachricht wird verwendet, um Kontakte über die Verschiebung einer \
     Session zu benachrichtigen."
  | MessageTemplateSignupVerification ->
    "Diese E-Mail wird verwendet, um neue E-Mail-Adressen nach der Anmeldung \
     zu verifizieren. Sie können die SMS-Texteingabe ignorieren."
  | MessageTemplateUserImport ->
    "Diese Nachricht informiert importierte Kontakte über die Migration zum \
     Z-Pool-Tool und enthält das Token, das sie zum Zurücksetzen ihres \
     Passworts benötigen."
  | MessageTemplateWaitingListConfirmation ->
    "Diese Nachricht bestätigt die erfolgreiche Eintragung in eine \
     Experiment-Warteliste."
  | MissingMessageTemplates ->
    "Die folgenden Nachrichtenvorlagen sind nicht vorhanden. Die \
     Standardnachricht wird an Kontakte gesendet, die eine dieser Sprachen als \
     ihre Kommunikationssprache ausgewählt haben."
  | NumberIsDaysHint -> "Anzahl Tage"
  | NumberIsSecondsHint -> "Anzahl Sekunden"
  | NumberIsWeeksHint -> "Anzahl Wochen"
  | NumberMax i -> error_to_string (Entity_message.NumberMax i)
  | NumberMin i -> error_to_string (Entity_message.NumberMin i)
  | Overbook ->
    "Anzahl Kontakte, die sich zusätzlich zur maximalen Anzahl Teilnehmer, an \
     einer Session einschreiben können."
  | PartialUpdate ->
    "Das folgende Formular wird die geänderten Werte sofort speichern. Sie \
     brauchen das Formular nicht abzuschicken."
  | ParticipationTagsHint ->
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
    "Zur gleichen Zeit laufen noch andere Versande. Siehe deren Details unten. \
     Wenn die Summe aller Versande das Maximum des Servers erreicht, werden \
     sie automatisch gleichmässig reduziert."
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
  | RescheduleSession ->
    "Wenn Sie eine Session verschieben werden alle an dieser Session \
     registrierten Kontakte automatisch informiert."
  | ResendRemindersChannel ->
    "Wenn Sie sich dafür entscheiden, die Erinnerungen als Textnachrichten zu \
     versenden, erhalten Kontakte, die keine verifizierte Handynummer haben, \
     die Erinnerung per E-Mail."
  | ResendRemindersWarning ->
    {sql|Es wurden noch keine automatischen Erinnerungen für diese Session verschickt. Stellen Sie sicher, dass das Nachrichtentemplate korrekt ist, falls Sie die Erinnerungen jetzt auslösen wollen.

Wenn Sie die Erinnerungen jetzt manuell auslösen werden über den gewählten Nachrichtenkanal keine automatischen Erinnerungen mehr verschickt.|sql}
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
  | RolePermissionsIntro ->
    {|Alle Berechtigungen, welche für Rollen des Tenants existieren.|}
  | ScheduleAt time ->
    time
    |> Pool_common_utils.Time.formatted_date_time
    |> Format.asprintf "Am %s"
  | ScheduledIntro ->
    {|Informationen über alle periodischen Hintergrund-Prozesse.

    Beachte: Wenn die Applikation neugestartet wird, werden alle auf "stopped" gesetzt|}
  | ScheduleEvery sec ->
    sec
    |> Pool_common_utils.Time.formatted_timespan
    |> Format.asprintf "alle %s"
  | SearchByFields fields ->
    Format.asprintf
      "Suche nach: %s"
      (fields |> CCList.map Locales_en.field_to_string |> CCString.concat ", ")
  | SelectedDateIsPast -> "Das gewählte Datum liegt in der Vergangenheit."
  | SelectedOptionsCountMax i ->
    error_to_string (Entity_message.SelectedOptionsCountMax i)
  | SelectedOptionsCountMin i ->
    error_to_string (Entity_message.SelectedOptionsCountMin i)
  | SessionCancellationMessageFollowUps ->
    "Dazugehörige Folgesessions wurden evenfalls abgesagt:"
  | SessionCancellationWithFollowups ->
    {|Wenn Sie diese Sitzung absagen, werden auch alle Folgesessions abgesagt.

Die folgenden Folgesessions existieren:|}
  | SessionCancelMessage ->
    "Dieser Grund wird allen angemeldeten Kontakten gezeigt."
  | SessionCloseHints ->
    Format.asprintf
      {|<strong>%s</strong> und <strong>%s</strong> schliessen sich gegenseitig aus.<br>
Wenn keine der Checkboxen angewählt ist, bedeutet das, dass der Kontakt erschienen ist, aber nicht teilgenommen hat.|}
      (Locales_de.field_to_string Entity_message_field.NoShow)
      (Locales_de.field_to_string Entity_message_field.Participated)
  | SessionCloseLegendNoShow ->
    "Der Kontakt ist nicht an der Session erschienen"
  | SessionCloseLegendParticipated ->
    "Der Kontakt hat am Experiment teilgenommen"
  | SessionCloseNoParticipationTagsSelected ->
    "Es wurden keine Tags ausgewählt, die den Teilnehmer/innen zugewiesen \
     werden, die an diesem Experiment teilgenommen haben."
  | SessionCloseParticipationTagsSelected ->
    "Die folgenden Tags werden allen Teilnehmer/innen zugewiesen, die an \
     diesem Experiment teilgenommen haben:"
  | SessionRegistrationFollowUpHint ->
    "Die Registrierung für eine Session inkl. allen Folgesessions ist \
     verbindlich."
  | SessionRegistrationHint ->
    "Die Registrierung für eine Session ist verbindlich."
  | SessionReminderLanguageHint ->
    "Falls sie einen eigenen Erinnerungstext angeben, wählen Sie dessen \
     Sprache hier."
  | SessionReminderLeadTime ->
    "Die Vorlaufzeit bestimmt, wie lange vor dem Start der Session die \
     Erinnerungen an die Kontakte verschickt wird."
  | SettingsNoEmailSuffixes ->
    "Es sind keine Email-Endungen definiert, die zugelassen sind. Das \
     bedeutet, dass alle Email-Endungen erlaubt sind."
  | SignUpForWaitingList ->
    "Das Rekrutierungsteam wird sich mit Ihnen in Verbindung setzen, um Ihnen \
     einen Termin zuzuweisen, wenn ein freier Platz vorhanden ist."
  | SmtpSettingsDefaultFlag ->
    "Achtung: Ist eine andere SMTP Konfiguration als Standard markiert, wird \
     diese Einstellung angepasst. Nur eine Konfiguration kann als Standard \
     markiert sein."
  | SmtpSettingsIntro ->
    {|Die folgende Konfiguration wird vom E-Mail Service verwendet.

    Beachte: Bei Verwendung des Mechanismus für "LOGIN" muss ein Benutzername und Passwort angegeben werden.|}
  | SwapSessions ->
    {|Das Ändern der Session wird nur diese Anmeldung anpassen. Wenn es Folgezuweisungen gibt, müssen diese manuell aktualisiert werden.

Es können nur Sitzungen mit freien Plätzen ausgewählt werden.|}
  | TagsIntro ->
    "Die definierten Tags können an die verschiedenen Objekte (z.B. Kontakte) \
     angehängt werden. Diese Tags können im Experiment-Filter verwendet werden \
     um sie ein- oder auszuschliessen."
  | TemplateTextElementsHint ->
    "Die folgenden Textbausteine können in den Templates verwendet werden. \
     Klicken Sie auf die Labels, um sie in die Zwischenablage zu kopieren."
  | TenantDatabaseLabel ->
    "Ein Label, das als Identifikator für den Tenant gilt, z.B. 'econ-uzh'. \
     Das Label muss einzigartig sein."
  | TenantDatabaseUrl ->
    {|Die Datenbank URL, nach folgendem Schema:
    mariadb://<user>:<pw>@<host>:<port>/<database>|}
  | TenantUrl -> "Die URL des Tenants ohne Protokoll, z.B.: pool.uzh.ch"
  | TestPhoneNumber ->
    "Bitte geben Sie eine Telefonnummer an, an die wir eine einzige \
     Testnachricht schicken können, um den API Key zu verifizieren. Die Nummer \
     muss im Format +41791234567 sein."
  | TextLengthMax i -> error_to_string (Entity_message.TextLengthMax i)
  | TextLengthMin i -> error_to_string (Entity_message.TextLengthMin i)
  | UserImportInterval ->
    {|<p>Legen Sie fest, nach wie vielen Tagen eine Erinnerung an Kontakte gesendet werden soll, die den Import noch nicht bestätigt haben.</p>
<p><strong>Die Einstellung "Zweite Erinnerung" legt fest, wie lange nach der ersten Erinnerung die zweite Erinnerung gesendet wird.</strong></p>|}
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
   | DeleteGtxApiKey ->
     ( "den GTX Api Key"
     , "löschen"
     , Some
         "Ohne einen hinterlegten GTX Api Key können keine Textnachrichten \
          mehr verschickt werden. Diese Aktion wird jeglichen Versand von \
          Textnachrichten deaktivieren." )
   | DeleteMailing -> "den Versand", "löschen", None
   | DeleteMessageTemplate -> "das Nachrichtentemplate", "löschen", None
   | DeleteSession -> "die Session", "löschen", None
   | DeleteSmtpServer -> "E-Mail Server", "löschen", None
   | LoadDefaultTemplate ->
     ( "das Standardtemplate"
     , "laden"
     , Some "Die aktuellen Inhalte werden dabei überschrieben." )
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
   | PublishCustomField ->
     ( "das Feld und alle dazugehörigen Optionen"
     , "publizieren"
     , Some "Sie werden das Feld nicht mehr löschen können." )
   | PublishCustomFieldOption ->
     ( "die Option"
     , "publizieren"
     , Some "Sie werden die Option nicht mehr löschen können." )
   | ReactivateAccount -> "den Account", "reaktivieren", None
   | RemovePermission -> "die Erlaubnis", "löschen", None
   | RemoveRule -> "die Regel", "löschen", None
   | RemoveTag -> "den Tag", "entfernen", None
   | RescheduleSession -> "die Session", "verschieben", None
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
