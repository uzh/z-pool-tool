open Sexplib.Conv

module Field = struct
  let go m fmt _ = Format.pp_print_string fmt m

  let custom _ fmt t =
    let _, name = t in
    Format.pp_print_string fmt name
  ;;

  type t =
    | Admin [@name "admin"] [@printer go "admin"]
    | AdminHint [@name "admin_hint"] [@printer go "admin_hint"]
    | AdminInputOnly [@name "admin_input_only"] [@printer go "admin_input_only"]
    | AdminViewOnly [@name "admin_view_only"] [@printer go "admin_view_only"]
    | Answer [@name "answer"] [@printer go "answer"]
    | AllowUninvitedSignup [@name "allow_uninvited_signup"]
        [@printer go "allow_uninvited_signup"]
    | AssetId [@name "asset_id"] [@printer go "asset_id"]
    | Assignment [@name "assignment"] [@printer go "assignment"]
    | Assignments [@name "assignments"] [@printer go "assignments"]
    | AssignmentCount [@name "assignment_count"]
        [@printer go "assignment_count"]
    | Assistants [@name "assistants"] [@printer go "assistants"]
    | Building [@name "building"] [@printer go "building"]
    | CanceledAt [@name "canceled_at"] [@printer go "canceled_at"]
    | City [@name "city"] [@printer go "city"]
    | ClosedAt [@name "closed_at"] [@printer go "closed_at"]
    | Comment [@name "comment"] [@printer go "comment"]
    | Contact [@name "contact"] [@printer go "contact"]
    | ContactEmail [@name "contact_email"] [@printer go "contact_email"]
    | Contacts [@name "contacts"] [@printer go "contacts"]
    | CreatedAt [@name "created_at"] [@printer go "created_at"]
    | CurrentPassword [@name "current_password"]
        [@printer go "current_password"]
    | CustomField [@name "custom_field"] [@printer go "custom_field"]
    | CustomFields [@name "custom_fields"] [@printer go "custom_fields"]
    | CustomFieldGroup [@name "custom_field_group"]
        [@printer go "custom_field_group"]
    | CustomFieldGroups [@name "custom_field_groups"]
        [@printer go "custom_field_groups"]
    | CustomFieldOption [@name "custom_field_option"]
        [@printer go "custom_field_option"]
    | CustomFieldOptions [@name "custom_field_options"]
        [@printer go "custom_field_options"]
    | CustomHtmx of (string * string) [@name "custom"]
        [@printer custom "custom"]
    | Database [@name "database"] [@printer go "database"]
    | DatabaseLabel [@name "database_label"] [@printer go "database_label"]
    | DatabaseUrl [@name "database_url"] [@printer go "database_url"]
    | Date [@name "date"] [@printer go "date"]
    | DateTime [@name "date_time"] [@printer go "date_time"]
    | DefaultLanguage [@name "default_language"]
        [@printer go "default_language"]
    | Description [@name "description"] [@printer go "description"]
    | DirectRegistrationDisabled [@name "direct_registration_disabled"]
        [@printer go "direct_registration_disabled"]
    | Disabled [@name "disabled"] [@printer go "disabled"]
    | Distribution [@name "distribution"] [@printer go "distribution"]
    | DistributionField [@name "distribution_field"]
        [@printer go "distribution_field"]
    | Duration [@name "duration"] [@printer go "duration"]
    | Email [@name "email"] [@printer go "email"]
    | EmailAddress [@name "email_address"] [@printer go "email_address"]
        [@printer go "default_language"]
    | EmailAddressUnverified [@name "email_address_unverified"]
        [@printer go "email_address_unverified"]
    | EmailAddressVerified [@name "email_address_verified"]
        [@printer go "email_address_verified"]
    | EmailSubject [@name "email_subject"] [@printer go "email_subject"]
    | EmailText [@name "email_text"] [@printer go "email_text"]
    | EmailSuffix [@name "email_suffix"] [@printer go "email_suffix"]
    | End [@name "end"] [@printer go "end"]
    | Experiment [@name "experiment"] [@printer go "experiment"]
    | ExperimentType [@name "experiment_type"] [@printer go "experiment_type"]
    | Experimenter [@name "experimenter"] [@printer go "experimenter"]
    | FieldType [@name "field_type"] [@printer go "field_type"]
    | File [@name "file"] [@printer go "file"]
    | FileMapping [@name "file_mapping"] [@printer go "file_mapping"]
    | FileMimeType [@name "file_mime_type"] [@printer go "file_mime_type"]
    | Filename [@name "filename"] [@printer go "filename"]
    | Filesize [@name "filesize"] [@printer go "filesize"]
    | Filter [@name "filter"] [@printer go "filter"]
    | Firstname [@name "firstname"] [@printer go "firstname"]
    | FollowUpSession [@name "follow_up_session"]
        [@printer go "follow_up_session"]
    | Hint [@name "hint"] [@printer go "hint"]
    | Host [@name "host"] [@printer go "host"]
    | I18n [@name "i18n"] [@printer go "i18n"]
    | Icon [@name "icon"] [@printer go "icon"]
    | Id [@name "id"] [@printer go "id"]
    | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
        [@printer go "inactive_user_disable_after"]
    | InactiveUserWarning [@name "inactive_user_warning"]
        [@printer go "inactive_user_warning"]
    | Institution [@name "institution"] [@printer go "institution"]
    | Interval [@name "interval"] [@printer go "interval"]
    | Invitation [@name "invitation"] [@printer go "invitation"]
    | InvitationCount [@name "invitation_count"]
        [@printer go "invitation_count"]
    | InvitationSubject [@name "invitation_subject"]
        [@printer go "invitation_subject"]
    | InvitationText [@name "invitation_text"] [@printer go "invitation_text"]
    | Invitations [@name "invitations"] [@printer go "invitations"]
    | Key [@name "key"] [@printer go "key"]
    | Label [@name "label"] [@printer go "label"]
    | Language [@name "language"] [@printer go "language"]
    | LanguageDe [@name "DE"] [@printer go "DE"]
    | LanguageEn [@name "EN"] [@printer go "EN"]
    | Lastname [@name "lastname"] [@printer go "lastname"]
    | LeadTime [@name "lead_time"] [@printer go "lead_time"]
    | Link [@name "link"] [@printer go "link"]
    | Location [@name "location"] [@printer go "location"]
    | LogoType [@name "logo_type"] [@printer go "logo_type"]
    | Mailing [@name "mailing"] [@printer go "mailing"]
    | MainSession [@name "main_session"] [@printer go "main_session"]
    | MaxParticipants [@name "max_participants"]
        [@printer go "max_participants"]
    | MessageChannel [@name "message_channel"] [@printer go "message_channel"]
    | MessageTemplate [@name "message_template"]
        [@printer go "message_template"]
    | MinParticipants [@name "min_participants"]
        [@printer go "min_participants"]
    | Model [@name "model"] [@printer go "model"]
    | Name [@name "name"] [@printer go "name"]
    | NewPassword [@name "new_password"] [@printer go "new_password"]
        [@printer go "num_invitations"]
    | Order [@name "order"] [@printer go "order"]
    | Operator [@name "operator"] [@printer go "operator"]
    | Operators [@name "operators"] [@printer go "operators"]
    | Overbook [@name "overbook"] [@printer go "overbook"]
    | Overwrite [@name "overwrite"] [@printer go "overwrite"]
    | Page [@name "page"] [@printer go "page"]
    | Participant [@name "participant"] [@printer go "participant"]
    | ParticipantCount [@name "participant_count"]
        [@printer go "participant_count"]
    | Participants [@name "participants"] [@printer go "participants"]
    | Participated [@name "participated"] [@printer go "participated"]
    | PartnerLogos [@name "partner_logos"] [@printer go "partner_logos"]
    | Predicate [@name "predicate"] [@printer go "predicate"]
    | Password [@name "password"] [@printer go "password"]
    | PasswordConfirmation [@name "password_confirmation"]
        [@printer go "password_confirmation"]
    | Paused [@name "paused"] [@printer go "paused"]
    | Profile [@name "profile"] [@printer go "profile"]
    | PublicTitle [@name "public_title"] [@printer go "public_title"]
    | PublishedAt [@name "published_at"] [@printer go "published_at"]
    | Query [@name "query"] [@printer go "query"]
    | RandomOrder [@name "random_order"] [@printer go "random_order"]
    | Rate [@name "rate"] [@printer go "rate"]
    | Reason [@name "reason"] [@printer go "reason"]
    | RegistrationDisabled [@name "registration_disabled"]
        [@printer go "registration_disabled"]
    | ReminderText [@name "reminder_text"] [@printer go "reminder_text"]
    | ReminderSubject [@name "reminder_subject"]
        [@printer go "reminder_subject"]
    | Required [@name "required"] [@printer go "required"]
    | ResentAt [@name "resent_at"] [@printer go "resent_at"]
    | Role [@name "role"] [@printer go "role"]
    | Room [@name "room"] [@printer go "room"]
    | Root [@name "root"] [@printer go "root"]
    | SentAt [@name "sent_at"] [@printer go "sent_at"]
    | Session [@name "session"] [@printer go "session"]
    | Sessions [@name "sessions"] [@printer go "sessions"]
    | Setting [@name "setting"] [@printer go "setting"]
    | ShowUp [@name "show_up"] [@printer go "show_up"]
    | SMS [@name "sms"] [@printer go "sms"]
    | SmsText [@name "sms_text"] [@printer go "sms_text"]
    | SmtpAuthMethod [@name "smtp_auth_method"] [@printer go "smtp_auth_method"]
    | SmtpAuthServer [@name "smtp_auth_server"] [@printer go "smtp_auth_server"]
    | SmtpPassword [@name "smtp_password"] [@printer go "smtp_password"]
    | SmtpPort [@name "smtp_port"] [@printer go "smtp_port"]
    | SmtpProtocol [@name "smtp_protocol"] [@printer go "smtp_protocol"]
    | SmtpReadModel [@name "smtp_read_model"] [@printer go "smtp_read_model"]
    | SmtpUsername [@name "smtp_username"] [@printer go "smtp_username"]
    | SmtpWriteModel [@name "smtp_write_model"] [@printer go "smtp_write_model"]
    | SortOrder [@name "sort_order"] [@printer go "sort_order"]
    | Start [@name "start"] [@printer go "start"]
    | Status [@name "status"] [@printer go "status"]
    | Street [@name "street"] [@printer go "street"]
    | Styles [@name "styles"] [@printer go "styles"]
    | Template [@name "template"] [@printer go "template"]
    | Tenant [@name "tenant"] [@printer go "tenant"]
    | TenantDisabledFlag [@name "tenant_disabled_flag"]
        [@printer go "tenant_disabled_flag"]
    | TenantId [@name "tenant_id"] [@printer go "tenant_id"]
    | TenantLogos [@name "tenant_logos"] [@printer go "tenant_logos"]
    | TenantMaintenanceFlag [@name "tenant_maintenance_flag"]
        [@printer go "tenant_maintenance_flag"]
    | TenantPool [@name "tenant_pool"] [@printer go "tenant_pool"]
    | TermsAccepted [@name "terms_accepted"] [@printer go "terms_accepted"]
    | TermsAndConditions [@name "terms_and_conditions"]
        [@printer go "terms_and_conditions"]
    | Time [@name "time"] [@printer go "time"]
    | TimeSpan [@name "timespan"] [@printer go "timespan"]
    | Title [@name "title"] [@printer go "title"]
    | Token [@name "token"] [@printer go "token"]
    | Translation [@name "translation"] [@printer go "translation"]
    | TriggerProfileUpdateAfter [@name "trigger_profile_update_after"]
        [@printer go "trigger_profile_update_after"]
    | Url [@name "url"] [@printer go "url"]
    | User [@name "user"] [@printer go "user"]
    | Value [@name "value"] [@printer go "value"]
    | Validation [@name "validation"] [@printer go "validation"]
    | Version [@name "version"] [@printer go "version"]
    | Virtual [@name "virtual"] [@printer go "virtual"]
    | WaitingList [@name "waiting_list"] [@printer go "waiting_list"]
    | Zip [@name "zip"] [@printer go "zip"]
        [@printer field_name "terms_and_conditions"]
  [@@deriving eq, show { with_path = false }, yojson, variants, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let url_key m = m |> show |> Format.asprintf ":%s"
  let array_key m = m |> show |> Format.asprintf "%s[]"
  let human_url m = m |> show |> CCString.replace ~sub:"_" ~by:"-"
end

(* TODO [aerben] make these general, compare what fields exist already, whenever
   pattern is "FIELD_ADJECTIVE", turn FIELD to Field.t and make it ADJECTIVE of
   Field.t *)
type error =
  | AccessDenied
  | AccessDeniedMessage
  | AllLanguagesRequired of Field.t
  | AlreadyInPast
  | AlreadyInvitedToExperiment of string list
  | AlreadyPublished of Field.t
  | AlreadySignedUpForExperiment
  | AlreadyStarted
  | Authorization of string
  | Conformist of (Field.t * error) list
  | ConformistModuleErrorType
  | ContactSignupInvalidEmail
  | ContactUnconfirmed
  | CustomFieldTypeChangeNotAllowed
  | Decode of Field.t
  | DecodeAction
  | DirectRegistrationIsDisabled
  | Disabled of Field.t
  | EmailAddressMissingAdmin
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailDeleteAlreadyVerified
  | EmailMalformed
  | EndBeforeStart
  | ExperimentSessionCountNotZero
  | FieldRequiresCheckbox of (Field.t * Field.t)
  | FilterMustNotContainTemplate
  | FilterAndOrMustNotBeEmpty
  | FilterListValueMustNotBeEmpty
  | FollowUpIsEarlierThanMain
  | HtmxVersionNotFound of string
  | Invalid of Field.t
  | InvalidEmailSuffix of string list
  | InvalidOptionSelected
  | InvalidHtmxRequest
  | LoginProvideDetails
  | MeantimeUpdate of Field.t
  | NegativeAmount
  | NoOptionSelected of Field.t
  | NotADatetime of (string * string)
  | NotANumber of string
  | NoTenantsRegistered
  | NotEligible
  | NotFound of Field.t
  | NotFoundList of Field.t * string list
  | NotHandled of string
  | NotInTimeRange
  | NoValue
  | NumberMax of int
  | NumberMin of int
  | Or of (error * error)
  | PasswordConfirmationDoesNotMatch
  | PasswordPolicy
  | PasswordResetFailMessage
  | PasswordResetInvalidData
  | PoolContextNotFound
  | PickMessageChannel
  | QueryNotCompatible of (Field.t * Field.t)
  | RegistrationDisabled
  | RequestRequiredFields
  | Retrieve of Field.t
  | SessionFullyBooked
  | SessionHasAssignments
  | SessionInvalid
  | SessionRegistrationViaParent
  | SessionTenantNotFound
  | ReadOnlyModel
  | RequiredFieldsMissing
  | SessionAlreadyCanceled of string
  | SessionAlreadyClosed of string
  | SessionInPast
  | SessionNotStarted
  | Smaller of (Field.t * Field.t)
  | TerminatoryRootError
  | TerminatoryRootErrorTitle
  | TerminatoryTenantError
  | TerminatoryTenantErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TextLengthMax of int
  | TextLengthMin of int
  | TimeInPast
  | TimeSpanPositive
  | TokenAlreadyUsed
  | TokenInvalidFormat
  | Undefined of Field.t
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants, sexp_of]

type warning = Warning of string
[@@deriving eq, show, yojson, variants, sexp_of]

let error_to_exn error = Failure (show_error error)

type success =
  | AddedToWaitingList
  | AssignmentCreated
  | Canceled of Field.t
  | Closed of Field.t
  | Created of Field.t
  | Deleted of Field.t
  | EmailConfirmationMessage
  | EmailVerified
  | FileDeleted
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | Published of Field.t
  | RemovedFromWaitingList
  | Rescheduled of Field.t
  | RoleAssigned
  | RoleDivested
  | SentList of Field.t
  | SettingsUpdated
  | Stopped of Field.t
  | TenantUpdateDatabase
  | TenantUpdateDetails
  | Updated of Field.t
[@@deriving eq, show, yojson, variants, sexp_of]

type info = Info of string [@@deriving eq, show, yojson, variants, sexp_of]

type t =
  | Message of string
  | PageNotFoundMessage
[@@deriving eq, show, yojson, variants, sexp_of]

let field_message prefix field suffix =
  Format.asprintf "%s %s %s" prefix field suffix
  |> CCString.trim
  |> CCString.capitalize_ascii
;;

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Field.Password
;;

type control =
  | Accept of Field.t option
  | Add of Field.t option
  | AddToWaitingList
  | Ascending
  | Assign of Field.t option
  | Back
  | Cancel of Field.t option
  | Choose of Field.t option
  | Close of Field.t option
  | Create of Field.t option
  | Decline
  | Delete of Field.t option
  | Descending
  | Disable
  | Divest of Field.t option
  | Duplicate of Field.t option
  | Edit of Field.t option
  | Enable
  | Enroll
  | Login
  | Manage of Field.t
  | More
  | PleaseSelect
  | Publish of Field.t option
  | Register
  | RemoveFromWaitingList
  | Reschedule of Field.t option
  | Resend of Field.t option
  | Save of Field.t option
  | Send of Field.t option
  | SendResetLink
  | SelectAll of Field.t option
  | SelectFilePlaceholder
  | Show
  | SignUp
  | Stop of Field.t option
  | Update of Field.t option
  | UpdateOrder
[@@deriving eq, show, yojson, variants, sexp_of]

let to_conformist_error error_list =
  CCList.map (fun (name, _, msg) -> name |> Field.read, msg) error_list
  |> conformist
;;

let add_field_query_params url params =
  let open CCList in
  let open Uri in
  map (CCPair.map_fst Field.show) params
  |> add_query_params' (of_string url)
  |> fun uri ->
  with_query uri (query uri |> rev |> uniq ~eq:Utils.equal_key |> rev)
  |> to_string
;;

module Collection = struct
  type t =
    { error : error list
    ; warning : warning list
    ; success : success list
    ; info : info list
    }
  [@@deriving eq, show, yojson, sexp_of]

  let empty = { error = []; warning = []; success = []; info = [] }
  let set_success txts message = { message with success = txts }
  let set_warning txts message = { message with warning = txts }
  let set_error txts message = { message with error = txts }
  let set_info txts message = { message with info = txts }

  let of_string str =
    let json =
      try Some (Yojson.Safe.from_string str) with
      | _ -> None
    in
    match json with
    | Some json -> Some (t_of_yojson json)
    | None -> None
  ;;

  let to_string t = yojson_of_t t |> Yojson.Safe.to_string
end
