open Entity_message

let field_to_string =
  let open Field in
  function
  | Admin -> "admin"
  | AdminInputOnly -> "input only by admins"
  | AdminViewOnly -> "only visible for admins"
  | AdminHint -> "hint for admins"
  | Answer -> "answer"
  | AllowUninvitedSignup -> "Allow sign up of uninvited contacts"
  | AssetId -> "asset identifier"
  | Assignment -> "assignment"
  | AssignmentCount -> "no. assignments"
  | Assignments -> "assignments"
  | Building -> "building"
  | CanceledAt -> "canceled at"
  | City -> "city"
  | ClosedAt -> "Closed at"
  | Comment -> "comment"
  | Contact -> "contact"
  | ContactEmail -> "contact email address"
  | Contacts -> "contacts"
  | CustomField -> "field"
  | CustomFieldGroup -> "group"
  | CustomFieldOption -> "option"
  | CreatedAt -> "created at"
  | CurrentPassword -> "current password"
  | CustomHtmx (label, _) -> label
  | Database -> "database"
  | DatabaseLabel -> "database label"
  | DatabaseUrl -> "database url"
  | Date -> "date"
  | DateTime -> "date and time"
  | DefaultLanguage -> "default language"
  | Description -> "description"
  | DirectRegistrationDisabled -> "direct registration disabled"
  | Disabled -> "disabled"
  | Distribution -> "distribution"
  | DistributionField -> "field"
  | Duration -> "duration"
  | Email -> "email address"
  | EmailAddress -> "email address"
  | EmailAddressUnverified -> "unverified email address"
  | EmailAddressVerified -> "verified email address"
  | EmailSuffix -> "email suffix"
  | End -> "end"
  | Experiment -> "experiment"
  | ExperimentType -> "experiment type"
  | FieldType -> "field type"
  | File -> "file"
  | FileMapping -> "file mapping"
  | FileMimeType -> "mime type"
  | Filename -> "filename"
  | Filesize -> "filesize"
  | Filter -> "filter"
  | Firstname -> "firstname"
  | FollowUpSession -> "follow-up session"
  | Hint -> "hint"
  | Host -> "host"
  | I18n -> "translation"
  | Icon -> "icon"
  | Id -> "identifier"
  | InactiveUserDisableAfter -> "disable inactive user after"
  | InactiveUserWarning -> "warn inactive user"
  | Institution -> "institution"
  | Interval -> "interval"
  | Invitation -> "invitation"
  | InvitationCount -> "no. invitations"
  | InvitationSubject -> "invitation subject"
  | InvitationText -> "invitation text"
  | Invitations -> "invitations"
  | Key -> "key"
  | Label -> "label"
  | Language -> "language"
  | LanguageDe -> "german"
  | LanguageEn -> "english"
  | Lastname -> "lastname"
  | Link -> "link"
  | Location -> "location"
  | LogoType -> "logo type"
  | LeadTime -> "lead time"
  | Mailing -> "mailing"
  | MainSession -> "main session"
  | MaxParticipants -> "maximum participants"
  | MinParticipants -> "minimum participants"
  | Model -> "model"
  | Name -> "name"
  | NewPassword -> "new password"
  | Order -> "order"
  | Operator -> "operator"
  | Operators -> "operators"
  | Overbook -> "overbook"
  | Overwrite -> "overwrite"
  | Page -> "page"
  | Participant -> "participant"
  | ParticipantCount -> "number of participants"
  | Participants -> "participants"
  | Participated -> "participated"
  | PartnerLogos -> "partner logos"
  | Password -> "password"
  | PasswordConfirmation -> "password confirmation"
  | Paused -> "paused"
  | Predicate -> "predicate"
  | Profile -> "profile"
  | PublicTitle -> "public title"
  | PublishedAt -> "published"
  | Query -> "query"
  | Rate -> "rate limit"
  | ReminderText -> "reminder text"
  | ReminderSubject -> "reminder subject"
  | RecruitmentChannel -> "recruitment channel"
  | RegistrationDisabled -> "registration disabled"
  | ResentAt -> "resent at"
  | Required -> "required"
  | Role -> "role"
  | Room -> "room"
  | Root -> "root"
  | Session -> "session"
  | Sessions -> "sessions"
  | Setting -> "setting"
  | ShowUp -> "show up"
  | SmtpAuthMethod -> "smtp authentication method"
  | SmtpAuthServer -> "smtp authentication server"
  | SmtpPassword -> "smtp password"
  | SmtpPort -> "smtp port"
  | SmtpProtocol -> "smtp protocol"
  | SmtpReadModel -> "smtp read model"
  | SmtpUsername -> "smtp username"
  | SmtpWriteModel -> "smtp write model"
  | SortOrder -> "sort order"
  | Start -> "start"
  | Status -> "status"
  | Street -> "street"
  | Styles -> "styles"
  | Template -> "template"
  | Tenant -> "tenant"
  | TenantDisabledFlag -> "disabled"
  | TenantId -> "tenant identifier"
  | TenantLogos -> "tenant logos"
  | TenantMaintenanceFlag -> "maintenance flag"
  | TenantPool -> "tenant pool"
  | TermsAccepted -> "accept"
  | TermsAndConditions -> "terms and conditions"
  | Time -> "time"
  | TimeSpan -> "time span"
  | Title -> "title"
  | Token -> "token"
  | Translation -> "translation"
  | TriggerProfileUpdateAfter -> "request to check the profile"
  | Url -> "url"
  | User -> "user"
  | Value -> "value"
  | Validation -> "validation"
  | Version -> "version"
  | Virtual -> "virtual"
  | WaitingList -> "waiting list"
  | Zip -> "zip code"
;;

let info_to_string : info -> string = function
  | Info s -> s
;;

let success_to_string : success -> string = function
  | AddedToWaitingList -> "You were added to the waiting list."
  | AssignmentCreated -> "You have been signed up successfully."
  | Canceled field ->
    field_message "" (field_to_string field) "was successfully canceled."
  | Closed field ->
    field_message "" (field_to_string field) "was successfully closed."
  | Created field ->
    field_message "" (field_to_string field) "was successfully created."
  | Deleted field ->
    field_message "" (field_to_string field) "was successfully deleted."
  | EmailConfirmationMessage ->
    "Successfully created. An email has been sent to your email address for  \
     verification."
  | EmailVerified -> "Email successfully verified."
  | FileDeleted -> "File was successfully deleted."
  | PasswordChanged -> "Password successfully changed."
  | PasswordReset -> "Password reset, you can now log in."
  | PasswordResetSuccessMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | Published field ->
    field_message "" (field_to_string field) "was successfully published."
  | RemovedFromWaitingList -> "You were removed from the waiting list."
  | Rescheduled field ->
    field_message "" (field_to_string field) "was successfully rescheduled."
  | SentList field ->
    field_message "" (field_to_string field) "were successfully sent."
  | SettingsUpdated -> "Settings were updated successfully."
  | Stopped field ->
    field_message "" (field_to_string field) "was successfully stopped."
  | TenantUpdateDatabase -> "Database information was successfully updated."
  | TenantUpdateDetails -> "Tenant was successfully updated."
  | Updated field ->
    field_message "" (field_to_string field) "was successfully updated."
;;

let warning_to_string : warning -> string = function
  | Warning string -> string
;;

let rec error_to_string = function
  | AllLanguagesRequired field ->
    field_message
      "Please provide '"
      (field |> field_to_string)
      "' in all languages."
  | AlreadyInPast -> "In minimum the starting point is in the past."
  | AlreadySignedUpForExperiment ->
    "You are already signed up for this experiment."
  | AlreadyStarted -> "Already started or ended, action not possible anymore."
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
  | ContactSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | ContactUnconfirmed -> "Participant isn't confirmed!"
  | Decode field -> field_message "Cannot decode" (field_to_string field) ""
  | DirectRegistrationIsDisabled ->
    "You cannot assign yourself to this experiment."
  | DecodeAction -> "Cannot decode action."
  | Disabled field -> field_message "" (field_to_string field) "is disabled."
  | EmailAddressMissingOperator -> "Please provide operator email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailAlreadyInUse -> "Email address is already in use."
  | EmailDeleteAlreadyVerified ->
    "Email address is already verified cannot be deleted."
  | EmailMalformed -> "Malformed email"
  | EndBeforeStart -> "End is before start time."
  | ExperimentSessionCountNotZero ->
    "Sessions exist for this experiment. It cannot be deleted."
  | FieldRequiresCheckbox (field, required) ->
    Format.asprintf
      "The option \"%s\" requires \"%s\"."
      (field_to_string field)
      (field_to_string required)
  | FilterMustNotContainTemplate -> "Filter must not contain templates."
  | FollowUpIsEarlierThanMain ->
    "Follow-up session can't start before main session."
  | HtmxVersionNotFound field ->
    Format.asprintf "No version found for field '%s'" field
  | Invalid field -> field_message "Invalid" (field_to_string field) "provided!"
  | InvalidEmailSuffix suffixes ->
    Format.asprintf
      "%s The following email suffixes are allowed: %s"
      (error_to_string (Invalid Field.EmailSuffix))
      (CCString.concat ", " suffixes)
  | InvalidOptionSelected -> "Invalid option selected."
  | InvalidHtmxRequest -> "Invalid request."
  | InvitationSubjectAndTextRequired ->
    "Please enter both a subject and a text for the session invitation."
  | LoginProvideDetails -> "Please provide email and password"
  | MeantimeUpdate field ->
    field_message "" (field_to_string field) "was updated in the meantime!"
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
  | PasswordConfirmationDoesNotMatch -> "The provided passwords don't match."
  | PasswordPolicy -> "Password doesn't match the required policy!"
  | PasswordResetFailMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PoolContextNotFound -> "Context could not be found."
  | QueryNotCompatible (f1, f2) ->
    Format.asprintf
      "%s is not compatible with %s."
      (field_to_string f1)
      (field_to_string f2)
  | ReadOnlyModel -> "Read only model!"
  | RegistrationDisabled -> "registration is disabled."
  | RequestRequiredFields -> "Please provide necessary fields"
  | Retrieve field -> field_message "Cannot retrieve" (field_to_string field) ""
  | SessionHasAssignments ->
    "There are already assignments for this session. It cannot be deleted."
  | SessionFullyBooked -> "Session is fully booked"
  | SessionInvalid -> "Invalid session, please login."
  | ReminderSubjectAndTextRequired ->
    "Please enter both a subject and a text for the session reminder."
  | RequiredFieldsMissing ->
    "To continue, you need to answer the following questions."
  | SessionTenantNotFound ->
    "Something on our side went wrong, please try again later or on multi  \
     occurrences please contact the Administrator."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s smaller than %s"
      (field_to_string field1)
      (field_to_string field2)
  | TerminatoryTenantError | TerminatoryRootError -> "Please try again later."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "An error occurred"
  | TermsAndConditionsMissing -> "Terms and conditions have to be added first."
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TextLengthMax i -> Format.asprintf "Must not be longer than %i." i
  | TextLengthMin i -> Format.asprintf "Must not be shorter than %i." i
  | TimeInPast -> "Time is in the past!"
  | TimeSpanPositive -> "Time span must be positive!"
  | TokenAlreadyUsed -> "The token was already used."
  | TokenInvalidFormat -> "Invalid Token Format!"
  | Undefined field -> field_message "Undefined" (field_to_string field) ""
  | WriteOnlyModel -> "Write only model!"
;;

let format_submit submit field =
  let field_opt_message f =
    f |> CCOption.map field_to_string |> CCOption.value ~default:""
  in
  field_message "" submit (field_opt_message field)
;;

let control_to_string = function
  | Accept field -> format_submit "accept" field
  | Add field -> format_submit "add" field
  | AddToWaitingList -> "Sign up for the waiting list"
  | Ascending -> format_submit "ascending" None
  | Assign field -> format_submit "assign" field
  | Back -> format_submit "back" None
  | Cancel field -> format_submit "cancel" field
  | Choose field -> format_submit "choose" field
  | Close field -> format_submit "close" field
  | Create field -> format_submit "create" field
  | Decline -> format_submit "decline" None
  | Delete field -> format_submit "delete" field
  | Descending -> format_submit "descending" None
  | Disable -> format_submit "disable" None
  | Edit field -> format_submit "edit" field
  | Enable -> format_submit "enable" None
  | Enroll -> format_submit "enroll" None
  | Login -> format_submit "login" None
  | Manage field -> format_submit "manage" (Some field)
  | More -> format_submit "more" None
  | PleaseSelect -> "please select"
  | Publish field -> format_submit "publish" field
  | RemoveFromWaitingList -> "Remove from waiting list"
  | Resend field -> format_submit "resend" field
  | Save field -> format_submit "save" field
  | SelectFilePlaceholder -> format_submit "select file.." None
  | Send field -> format_submit "send" field
  | Reschedule field -> format_submit "reschedule" field
  | SendResetLink -> format_submit "send reset link" None
  | Show -> format_submit "show" None
  | SignUp -> format_submit "sign up" None
  | Stop field -> format_submit "stop" field
  | Update field -> format_submit "update" field
  | UpdateOrder -> format_submit "update order" None
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "The requested page could not be found."
;;
