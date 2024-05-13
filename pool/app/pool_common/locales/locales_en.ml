module Ptime = Utils.Ptime
open Pool_message

let rec field_to_string =
  let open Field in
  let combine one two =
    CCString.concat ": " [ field_to_string one; field_to_string two ]
  in
  function
  | Action -> "action"
  | Active -> "active"
  | ActiveContactsCount -> "active contacts count"
  | Actor -> "actor"
  | Admin -> "admin"
  | AdminComment -> "admin comment"
  | AdminInput -> "admin input"
  | AdminHint -> "hint for admins"
  | AdminInputOnly -> "input only by admins"
  | AdminViewOnly -> "only visible for admins"
  | AllowUninvitedSignup -> "Allow registration of all contacts"
  | Answer -> "answer"
  | AreaCode -> "area code"
  | Argument -> "argument"
  | AssetId -> "asset identifier"
  | AssignableRole -> "assignable role"
  | Assignment -> "assignment"
  | AssignmentCount -> "no. assignments"
  | Assignments -> "assignments"
  | AssignmentsCreated -> "assignments created"
  | AssignmentWithoutSession -> "assignment without session"
  | Assistants -> "assistants"
  | AvailableLanguages -> "available languages"
  | Building -> "building"
  | CanceledAt -> "canceled at"
  | CallbackUrl -> "callback url"
  | CellPhone -> "cell phone"
  | Chronological -> "chronological"
  | City -> "city"
  | ClosedAt -> "Closed at"
  | Confirmed -> "Confirmed"
  | ConfirmedAt -> "Confirmed at"
  | Contact -> "contact"
  | ContactCount -> "No. contacts"
  | ContactEmail -> "contact email address"
  | ContactLanguage -> "Contact & display language"
  | ContactPerson -> "contact person"
  | Contacts -> "contacts"
  | Context -> "context"
  | CostCenter -> "cost center"
  | Count -> "count"
  | CreatedAt -> "created at"
  | CurrentPassword -> "current password"
  | CustomField -> "field"
  | CustomFieldGroup -> "group"
  | CustomFieldGroups -> "groups"
  | CustomFieldOption -> "option"
  | CustomFieldOptions -> "options"
  | CustomFields -> "fields"
  | CustomHtmx (label, _) -> label
  | Database -> "database"
  | DatabaseLabel -> "database label"
  | DatabaseUrl -> "database url"
  | Date -> "date"
  | DateTime -> "date and time"
  | DefaultLanguage -> "default language"
  | DefaultSmtpServer -> "default server"
  | Description -> "description"
  | DirectRegistrationDisabled -> "direct registration disabled"
  | Disabled -> "disabled"
  | Distribution -> "distribution"
  | DistributionField -> "field"
  | Duration -> "duration"
  | Email -> "email"
  | EmailAddress -> "email address"
  | EmailAddressUnverified -> "unverified email address"
  | EmailAddressVerified -> "verified email address"
  | EmailLeadTime -> "email lead time"
  | EmailRemindersSentAt -> "email reminders sent at"
  | EmailSubject -> "email subject"
  | EmailSuffix -> "email suffix"
  | EmailText -> "email text"
  | EmailsSent -> "total emails sent"
  | End -> "end"
  | Exclude -> "exclude"
  | ExcludeRolesOf -> "exclude roles of"
  | Experiment -> "experiment"
  | ExperimentCount -> "no. experiments"
  | Experiments -> "experiments"
  | ExperimentEmailReminderLeadTime ->
    Format.asprintf "experiment specific email %s" (field_to_string LeadTime)
  | ExperimentTextMessageReminderLeadTime ->
    Format.asprintf
      "experiment specific text message %s"
      (field_to_string LeadTime)
  | ExperimentType -> "experiment type"
  | Experimenter -> "experimenter"
  | ExternalDataIdAbbr -> "EID"
  | ExternalDataId -> "external data identifier"
  | ExternalDataRequired -> "external data is required"
  | Failed -> "failed"
  | FallbackToEmail ->
    "Do you wish to send the message as an email to contacts without a cell \
     phone?"
  | FieldType -> "field type"
  | File -> "file"
  | FileMapping -> "file mapping"
  | FileMimeType -> "mime type"
  | Filename -> "filename"
  | Filesize -> "filesize"
  | Filter -> "filter"
  | Firstname -> "firstname"
  | FirstReminder -> "first reminder"
  | FollowUpSession -> "follow-up session"
  | GtxApiKey -> "GTX Api Key"
  | HideCanceled -> "Hide canceled"
  | HideClosed -> "Hide closed"
  | HideMakedAsDeleted -> "Hide marked as deleted"
  | HidePaused -> "Hide paused"
  | HideInactive -> "Hide inactive"
  | HidePast -> "Hide past"
  | HideUnverified -> "Hide unverified"
  | Hint -> "hint"
  | Host -> "host"
  | I18n -> "translation"
  | Icon -> "icon"
  | Id -> "identifier"
  | ImportPending -> "import pending"
  | InactiveUserDisableAfter -> "disable inactive user after"
  | InactiveUserWarning -> "warn inactive user"
  | Inactive -> "inactive"
  | Input -> "input"
  | Institution -> "institution"
  | InternalDescription -> "internal description"
  | Interval -> "interval"
  | Invitation -> "invitation"
  | InvitationCount -> "no. invitations"
  | InvitationResetAt -> "invitation reset at"
  | Invitations -> "invitations"
  | InvitationSubject -> "invitation subject"
  | InvitationsSent -> "invitations sent"
  | InvitationText -> "invitation text"
  | IsAdmin -> "is admin"
  | Key -> "key"
  | Label -> "label"
  | Language -> "language"
  | LanguageDe -> "german"
  | LanguageEn -> "english"
  | LastError -> "last error message"
  | LastErrorAt -> "last error"
  | LastManuallyRemindedAt -> "Last manually reminded at"
  | Lastname -> "lastname"
  | LastRun -> "last run"
  | LastRunAt -> "last run"
  | LeadTime -> "lead time"
  | Limit -> "Limit"
  | Link -> "link"
  | Location -> "location"
  | Locations -> "locations"
  | LoginCount -> "contact logins"
  | LogoType -> "logo type"
  | Mailing -> "mailing"
  | MainSession -> "main session"
  | MarkedAsDeleted -> "marked as deleted"
  | MatchingFilterCount -> "Number of contacts meeting criteria"
  | MaxParticipants -> "maximum participants"
  | MaxTries -> "maximum tries"
  | Message -> "message"
  | MessageChannel -> "message channel"
  | MessageTemplate -> "message template"
  | MessageTemplates -> "message templates"
  | MinParticipants -> "minimum participants"
  | Model -> "model"
  | Name -> "name"
  | NewPassword -> "new password"
  | NextRunAt -> "next run"
  | NoShow -> "no show"
  | NoShowAbr -> "NS"
  | NoShowCount -> "no shows"
  | NotMatchingFilterCount -> "not matching filter"
  | NotifiedAt -> "notified at"
  | NotifyVia -> "notify via"
  | NotifyContact -> "notify contact"
  | Offset -> "offset"
  | OnlineExperiment -> "online experiment"
  | Operator -> "operator"
  | Operators -> "operators"
  | Order -> "order"
  | OrganisationalUnit -> "organisational unit"
  | Overbook -> "overbook"
  | OverriddenValue -> "overriden contact answer"
  | Override -> "override"
  | Page -> "page"
  | PageCount -> "nr of pages"
  | Participant -> "participant"
  | ParticipantCount -> "participants"
  | Participants -> "participants"
  | Participated -> "participated"
  | ParticipatedAbr -> "P"
  | ParticipationTag -> "participation tag"
  | ParticipationTags -> "participation tags"
  | PartnerLogos -> "partner logos"
  | Password -> "password"
  | PasswordConfirmation -> "password confirmation"
  | Paused -> "paused"
  | PendingContactImports -> "pending contact imports"
  | Period -> "period"
  | Permission -> "permission"
  | PermissionOn (role, target) ->
    Format.asprintf "permission %s of %s" target role
  | PlainText -> "plaintext"
  | Predicate -> "predicate"
  | PromptOnRegistration -> "promt during registration"
  | Profile -> "profile"
  | PublicDescription -> "public description"
  | PublicTitle -> "public title"
  | PublishedAt -> "published"
  | Query -> "query"
  | Queue -> "queue"
  | RandomOrder -> "select the contacts in random order."
  | Reason -> "reason"
  | Recipient -> "recipient"
  | Redirect -> "redirect"
  | Reminder -> "reminder"
  | RegistrationDisabled -> "registration disabled"
  | RegistrationPossible -> "registration possible"
  | LastRemindedAt -> "reminded at"
  | ReminderCount -> "nr of reminders"
  | RemindersSent -> "reminders sent"
  | Required -> "required"
  | ResentAt -> "resent at"
  | Role -> "role"
  | Room -> "room"
  | Root -> "root"
  | Rule -> "rule"
  | ScheduledTime -> "scheduled time"
  | ScheduledTimeSpan -> "scheduled interval"
  | Search -> "search"
  | SearchOf field -> combine Search field
  | SecondReminder -> "second reminder"
  | Sender -> "sender"
  | SendingInvitations -> "Sending invitations"
  | SentAt -> "sent at"
  | SessionCount -> "no. sessions"
  | Session -> "session"
  | Sessions -> "sessions"
  | Setting -> "setting"
  | Settings -> "settings"
  | ShowUpCount -> "show ups"
  | ShowExteralDataIdLinks -> "show links to external data identifiers"
  | SignedUpAt -> "signed up at"
  | SignUpCount -> "new sign ups"
  | SMS -> "SMS"
  | SmsText -> "SMS text"
  | Smtp -> "smtp"
  | SmtpLabel -> "label"
  | SmtpMechanism -> "mechanism"
  | SmtpPassword -> "password"
  | SmtpPort -> "port"
  | SmtpProtocol -> "protocol"
  | SmtpServer -> "server"
  | SmtpUsername -> "username"
  | SortOrder -> "sort order"
  | Start -> "start"
  | StartNow -> "start now"
  | Status -> "status"
  | Street -> "street"
  | Styles -> "styles"
  | Successful -> "successful"
  | Survey -> "survey"
  | SurveyUrl -> "survey url"
  | SystemEvent -> "system event"
  | Tag -> "tag"
  | Tags -> "tags"
  | Tagging -> "tagging"
  | Target -> "target"
  | Template -> "template"
  | Tenant -> "tenant"
  | TenantDisabledFlag -> "disabled"
  | TenantId -> "tenant identifier"
  | TenantLogos -> "tenant logos"
  | TenantMaintenanceFlag -> "maintenance flag"
  | TenantPool -> "tenant pool"
  | TermsAccepted -> "accept"
  | TermsAcceptedCount -> "terms and conditions accepted"
  | TermsAndConditions -> "terms and conditions"
  | TestPhoneNumber -> "test phone number"
  | TextMessage -> "text message"
  | TextMessageLeadTime -> "text message lead time"
  | TextMessageRemindersSentAt -> "text message reminders sent at"
  | Time -> "time"
  | TimeWindow -> "time window"
  | TimeSpan -> "time span"
  | TimeUnit -> "time unit"
  | TimeUnitOf field -> combine TimeUnit field
  | Title -> "title"
  | ToHandle -> "to handle"
  | Token -> "token"
  | Total -> "total"
  | Translation -> "translation"
  | Tries -> "tries"
  | TriggerProfileUpdateAfter -> "request to check the profile"
  | UpdatedAt -> "updated at"
  | Url -> "url"
  | User -> "user"
  | Validation -> "validation"
  | Value -> "value"
  | ValueOf field -> combine Value field
  | VerificationCode -> "verification code"
  | Verified -> "verified"
  | Version -> "version"
  | Virtual -> "virtual"
  | WaitingList -> "waiting list"
  | Year -> "year"
  | Zip -> "zip code"
;;

let info_to_string =
  let open Info in
  function
  | Info s -> s
;;

let success_to_string =
  let open Success in
  function
  | AddedToWaitingList -> "You were added to the waiting list."
  | AssignmentCreated -> "You have been signed up successfully."
  | Canceled field ->
    field_message "" (field_to_string field) "was successfully canceled."
  | Closed field ->
    field_message "" (field_to_string field) "was successfully closed."
  | ContactPromoted -> "The contact was successfully promoted to an admin."
  | Created field ->
    field_message "" (field_to_string field) "was successfully created."
  | Deleted field ->
    field_message "" (field_to_string field) "was successfully deleted."
  | EmailConfirmationMessage ->
    "An email has been sent to your email address for verification if the \
     given email address is still available."
  | EmailUpdateConfirmationMessage ->
    {|If the email address you entered is still available, an email with a confirmation link has been sent to this address. Please confirm the address by opening this link.

As long as the new e-mail address has not been confirmed, the current address will remain in use.|}
  | EmailVerified -> "Email successfully verified."
  | FileDeleted -> "File was successfully deleted."
  | ImportCompleted ->
    "The import of your account has successfully been completed."
  | MarkedAsDeleted field ->
    field_message "" (field_to_string field) "was marked as deleted."
  | PausedToggled paused ->
    Format.asprintf
      "The account was successfully %s."
      (if paused then "paused" else "reactivated")
  | PasswordChanged -> "Password successfully changed."
  | PasswordReset -> "Password reset, you can now log in."
  | PasswordResetSuccessMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | CellPhoneTokenSent ->
    "A text message has been sent to your phone for verification. Please enter \
     the provided code."
  | CellPhoneVerified -> "Your phone number has successfully been verified."
  | Published field ->
    field_message "" (field_to_string field) "was successfully published."
  | RemovedFromWaitingList -> "You were removed from the waiting list."
  | RemindersResent -> "The reminders have been resent."
  | Rescheduled field ->
    field_message "" (field_to_string field) "was successfully rescheduled."
  | Resent field ->
    field_message "" (field_to_string field) "was successfully resent."
  | ResetInvitations ->
    "Reset Invitations. In upcoming mailings, previous invitations will be \
     considered to reinvite."
  | RoleAssigned -> "Role was assigned."
  | RoleUnassigned -> "Role was unassigned."
  | SentList field ->
    field_message "" (field_to_string field) "were successfully sent."
  | Sent field ->
    field_message "" (field_to_string field) "was successfully sent."
  | SettingsUpdated -> "Settings were updated successfully."
  | SmtpConfigurationAdded -> "The SMTP configuration was added successfully."
  | SmtpDetailsUpdated -> "SMTP settings successfully updated."
  | SmtpPasswordUpdated -> "SMTP password successfully updated."
  | Stopped field ->
    field_message "" (field_to_string field) "was successfully stopped."
  | TagAssigned -> "The tag was successfully assigned."
  | TagRemoved -> "The tag is removed."
  | TenantUpdateDatabase -> "Database information was successfully updated."
  | TenantUpdateDetails -> "Tenant was successfully updated."
  | Updated field ->
    field_message "" (field_to_string field) "was successfully updated."
  | Validated field ->
    field_message "" (field_to_string field) "was successfully validated."
  | VerificationMessageResent -> "The verification message has been resent."
;;

let warning_to_string =
  let open Warning in
  function
  | Warning string -> string
;;

let rec error_to_string =
  let open Error in
  function
  | AccountTemporarilySuspended ptime ->
    ptime
    |> Ptime.formatted_date_time
    |> Format.asprintf
         "Too many failed login attempts. This email address is blocked until \
          %s"
  | AccessDenied -> "Access denied"
  | AccessDeniedMessage -> "Access to the requested page is denied"
  | AllLanguagesRequired field ->
    field_message
      "Please provide '"
      (field |> field_to_string |> CCString.trim)
      "' in all languages."
  | AlreadyExisting field ->
    field_message
      "The field data for the '"
      (field |> field_to_string |> CCString.trim)
      "' already exists."
  | AlreadyInPast -> "In minimum the starting point is in the past."
  | AlreadySignedUpForExperiment ->
    "You are already signed up for this experiment."
  | AssignmentIsCanceled -> "Assignment was canceled."
  | AssignmentIsClosed -> "Assignment is already closed."
  | AssignmentsHaveErrors ->
    "Some assignments have errors. Please resolve them first."
  | AlreadyStarted -> "Already started or ended, action not possible anymore."
  | AssignmentAlreadySubmitted -> "This assignment was already submitted."
  | AlreadyInvitedToExperiment names ->
    Format.asprintf
      "The following contacts have already been invited to this experiment: %s"
      (CCString.concat ", " names)
  | AlreadyPublished field ->
    field_message
      ""
      (field |> field_to_string |> CCString.trim)
      "has alredy been published."
  | Authorization message -> field_message "Unable to authorize: " message ""
  | CannotBeDeleted field ->
    Format.asprintf "%s cannot be deleted." (field_to_string field)
  | CannotBeUpdated field ->
    Format.asprintf "%s cannot be updated." (field_to_string field)
  | CaqtiError err -> err
  | Connection err -> [%string "Connection error: %{err}"]
  | Conformist errs ->
    CCList.map
      (fun (field, err) ->
        Format.asprintf
          "%s: %s"
          (field_to_string field |> CCString.capitalize_ascii)
          (error_to_string err))
      errs
    |> CCString.concat "\n"
  | ConformistModuleErrorType -> failwith "Do not use"
  | ContactDoesNotMatchFilter ->
    "The contact does not meet the criteria specified in the filter."
  | ContactExperimentNotFound ->
    "Currently, there are no free spots to participate in this experiment."
  | ContactIsInactive -> "This contact is inactive."
  | ContactSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | ContactUnconfirmed -> "Participant isn't confirmed!"
  | CustomFieldNoOptions -> "At least one option must exist."
  | CustomFieldTypeChangeNotAllowed -> "Type of field cannot be changed."
  | DatabaseAddPoolFirst -> "Unknown Pool: Please 'add_pool' first!"
  | Decode field -> field_message "Cannot decode" (field_to_string field) ""
  | DirectRegistrationIsDisabled ->
    "You cannot assign yourself to this experiment."
  | DecodeAction -> "Cannot decode action."
  | DefaultMustNotBeUnchecked -> "'Default' must not be unchecked."
  | Disabled field -> field_message "" (field_to_string field) "is disabled."
  | EmailAddressMissingAdmin -> "Please provide admin email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailAlreadyInUse -> "Email address is already in use."
  | EmailDeleteAlreadyVerified ->
    "Email address is already verified cannot be deleted."
  | EmailIdenticalToCurrent ->
    "The provided email address is identical to the current one."
  | EmailMalformed -> "Malformed email"
  | EmailInterceptionError error ->
    Format.asprintf "Email interception error: %s" error
  | EndBeforeStart -> "End is before start time."
  | ExperimentSessionCountNotZero ->
    "Sessions exist for this experiment. It cannot be deleted."
  | FieldRequired field ->
    Format.asprintf
      "%s is required."
      (field_to_string field |> CCString.capitalize_ascii)
  | FilterMustNotContainTemplate -> "Filter must not contain templates."
  | FilterAndOrMustNotBeEmpty -> "'And' and 'Or' predicates must not be empty."
  | FilterListValueMustNotBeEmpty -> "At least one option must be selected."
  | FollowUpIsEarlierThanMain ->
    "Follow-up session can't start before main session."
  | HtmxVersionNotFound field ->
    Format.asprintf "No version found for field '%s'" field
  | ImportPending ->
    "The import of your user is not completed yet. Please check your inbox or \
     contact an administrator."
  | Invalid field -> field_message "Invalid" (field_to_string field) "provided!"
  | InvalidEmailSuffix suffixes ->
    Format.asprintf
      "%s The following email suffixes are allowed: %s"
      (error_to_string (Invalid Field.EmailSuffix))
      (CCString.concat ", " suffixes)
  | InvalidJson exn -> Format.asprintf "Invalid Json: %s" exn
  | InvalidOptionSelected -> "Invalid option selected."
  | InvalidPasswordHashingCount ->
    "Password hashing count has to be between 4 and 31"
  | InvalidRequest | InvalidHtmxRequest -> "Invalid request."
  | IsMarkedAsDeleted field ->
    field_message
      ""
      (field |> field_to_string |> CCString.trim)
      "has been marked as deleted."
  | JobCannotBeRetriggered -> "This job cannot be retriggered."
  | JobPending -> "The job is still pending."
  | LoginProvideDetails -> "Please provide email and password"
  | MeantimeUpdate field ->
    field_message "" (field_to_string field) "was updated in the meantime!"
  | Missing field ->
    field_message
      "The field"
      (field_to_string field)
      "is missing or not filled out."
  | MutuallyExclusive (f1, f2) ->
    Format.asprintf
      "'%s' and '%s' are mutually exclusive."
      (field_to_string f1)
      (field_to_string f2)
  | NegativeAmount -> "Has negative amount!"
  | NoOptionSelected field ->
    field_message "Please select at least one" (field_to_string field) "."
  | NotADatetime (time, err) ->
    Format.asprintf "%s: '%s' is not a valid date or time." err time
  | NotANumber field -> Format.asprintf "'%s' is not a number." field
  | NoTenantsRegistered -> "There are no tenants registered in root database!"
  | NotEligible -> "Your are not eligible to perform this action."
  | NotFound field -> field_message "" (field_to_string field) "not found!"
  | NotFoundList (field, items) ->
    field_message
      "Following"
      (field_to_string field)
      (Format.asprintf "could not be found: %s" (CCString.concat "," items))
  | NotHandled field -> Format.asprintf "Field '%s' is not handled." field
  | NotInTimeRange -> "Not in specified time slot."
  | NoValue -> "No value provided."
  | NumberMax i -> Format.asprintf "Must not be larger than %i." i
  | NumberMin i -> Format.asprintf "Must not be smaller than %i." i
  | Or (err1, err2) ->
    CCFormat.asprintf
      "%s or %s"
      (error_to_string err1)
      (err2 |> error_to_string |> CCString.uncapitalize_ascii)
  | PasswordConfirmationDoesNotMatch -> "The provided passwords don't match."
  | PasswordPolicyMinLength n ->
    Format.asprintf "The password must at least contain %i characters." n
  | PasswordPolicyCapitalLetter -> "The password must contain a capital letter."
  | PasswordPolicyNumber -> "The password must contain a number."
  | PasswordPolicySpecialChar chars ->
    Format.asprintf
      "The password must contain one of the following characters: %s"
      (chars |> CCList.map CCString.of_char |> CCString.concat " ")
  | PasswordResetFailMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PermissionDeniedCreateRule -> "Permission denied to create the rule"
  | PermissionDeniedGrantRole -> "Permission denied to grant the role"
  | PermissionDeniedRevokeRole -> "Permission denied to revoke the role"
  | PickMessageChannel ->
    "No message channel has been selected for the notification of contacts."
  | PoolContextNotFound -> "Context could not be found."
  | QueryNotCompatible (f1, f2) ->
    Format.asprintf
      "%s is not compatible with %s."
      (field_to_string f1)
      (field_to_string f2)
  | ReadOnlyModel -> "Read only model!"
  | RegistrationDisabled -> "registration is disabled."
  | RequestRequiredFields -> "Please provide necessary fields"
  | RequiredFieldsMissing ->
    "To continue, you need to answer the following questions."
  | Retrieve field -> field_message "Cannot retrieve" (field_to_string field) ""
  | SessionFullyBooked -> "Session is fully booked"
  | SessionHasAssignments ->
    "There are already assignments for this session. It cannot be deleted."
  | SessionHasFollowUps ->
    "There is already a follow-up session for this session. It cannot be \
     deleted."
  | SessionInvalid -> "Invalid session, please login."
  | SelectedOptionsCountMax i ->
    Format.asprintf
      "A maximum of %i %s may be selected."
      i
      (if i == 1 then "option" else "options")
  | SelectedOptionsCountMin i ->
    Format.asprintf
      "A minimum of %i %s to be selected."
      i
      (if i == 1 then "option has" else "options have")
  | SessionAlreadyCanceled date ->
    CCFormat.asprintf "This session has already been canceled on %s." date
  | SessionAlreadyClosed date ->
    CCFormat.asprintf "This session has already been closed at %s." date
  | SessionNotClosed -> "This session has not been closed yet."
  | SessionInPast -> "This session has already finished."
  | SessionNotStarted -> "This session cannot be closed, yet."
  | SessionRegistrationViaParent -> "Registration via main session."
  | SessionTenantNotFound ->
    "Missing tenant: something on our side went wrong, please try again later \
     or on multi occurrences please contact the Administrator."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s smaller than %s"
      (field_to_string field1)
      (field_to_string field2)
  | SessionOverlap -> "This time window overlaps with another."
  | SmtpException exn -> exn
  | SmtpLoginMissingCredentials ->
    "SMTP auth mechanism cannot be set to LOGIN when no username or password \
     is set."
  | TerminatoryTenantError | TerminatoryRootError -> "Please try again later."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "An error occurred"
  | TermsAndConditionsMissing -> "Terms and conditions have to be added first."
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TextLengthMax i ->
    Format.asprintf "Must not be longer than %i characters." i
  | TextLengthMin i ->
    Format.asprintf "Must not be shorter than %i characters." i
  | TextMessageInterceptionError error ->
    Format.asprintf "Text message interception error: %s" error
  | TimeInPast -> "Time is in the past!"
  | TimeSpanPositive -> "Time span must be positive!"
  | TokenAlreadyUsed -> "The token was already used."
  | TokenInvalidFormat -> "Invalid Token Format!"
  | TooShort -> "The duration specified is too short."
  | Undefined field -> field_message "Undefined" (field_to_string field) ""
  | Uniqueness field ->
    field_message "" (field_to_string field) "must be unique."
  | Unsupported text -> [%string "'%{text}' is unsupported."]
  | WriteOnlyModel -> "Write only model!"
;;

let format_submit submit field =
  let field_opt_message f =
    f |> CCOption.map field_to_string |> CCOption.value ~default:""
  in
  field_message "" submit (field_opt_message field)
;;

let control_to_string =
  let open Control in
  function
  | Accept field -> format_submit "accept" field
  | Add field -> format_submit "add" field
  | AddToWaitingList -> "Sign up for the waiting list"
  | Ascending -> format_submit "ascending" None
  | Apply -> "apply"
  | Assign field -> format_submit "assign" field
  | Back -> format_submit "back" None
  | Cancel field -> format_submit "cancel" field
  | ChangeSession -> format_submit "change" (Some Field.Session)
  | Choose field -> format_submit "choose" field
  | Close field -> format_submit "close" field
  | Create field -> format_submit "create" field
  | Decline -> format_submit "decline" None
  | Delete field -> format_submit "delete" field
  | Details -> format_submit "details" None
  | Descending -> format_submit "descending" None
  | Disable -> format_submit "disable" None
  | Duplicate field -> format_submit "duplicate" field
  | Edit field -> format_submit "edit" field
  | Enable -> format_submit "enable" None
  | Enroll -> format_submit "enroll" None
  | EnterNewCellPhone -> "Enter a different number"
  | Filter field -> format_submit "filter" field
  | Login -> format_submit "login" None
  | LoadDefaultTemplate -> format_submit "load default template" None
  | Manage field -> format_submit "manage" (Some field)
  | MarkAsDeleted -> format_submit "mark as deleted" None
  | More -> format_submit "more" None
  | NextPage -> "next"
  | OpenProfile -> "show profile"
  | PauseAccount -> format_submit "pause account" None
  | PleaseSelect -> format_submit "please select" None
  | PreviousPage -> "previous"
  | PromoteContact -> format_submit "promote contact" None
  | Print field -> format_submit "print" field
  | PublicPage -> "show public page"
  | Publish field -> format_submit "publish" field
  | ReactivateAccount -> format_submit "reactivate account" None
  | Register -> format_submit "register" None
  | RemoveFromWaitingList -> "Remove from waiting list"
  | Remove field -> format_submit "remove" field
  | Reschedule field -> format_submit "reschedule" field
  | Resend field -> format_submit "resend" field
  | Reset field -> format_submit "reset" field
  | ResetForm -> "reset form"
  | ResetPlainText ->
    Format.asprintf
      "Reset %s to rich '%s'"
      (field_to_string Field.PlainText)
      (field_to_string Field.EmailText)
  | Resume field -> format_submit "resume" field
  | Save field -> format_submit "save" field
  | SessionDetails -> format_submit "session details" None
  | Select -> format_submit "select" None
  | SelectAll field -> format_submit "select all" field
  | SelectFilePlaceholder -> format_submit "select file.." None
  | Send field -> format_submit "send" field
  | SendResetLink -> format_submit "send reset link" None
  | Show -> format_submit "show" None
  | SignUp -> format_submit "sign up" None
  | Start field -> format_submit "start" field
  | Stop field -> format_submit "stop" field
  | ToggleAll -> "toggle all"
  | Unassign field -> format_submit "unassign" field
  | Update field -> format_submit "update" field
  | UpdateAssignmentsMatchFilter -> format_submit "rerun filter" None
  | UpdateOrder -> format_submit "update order" None
  | Validate -> format_submit "validate" None
  | Verify field -> format_submit "verify" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "The requested page could not be found."
;;
