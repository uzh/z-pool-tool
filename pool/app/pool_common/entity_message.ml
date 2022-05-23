open Sexplib.Conv

module Field = struct
  let field_name m fmt _ = Format.pp_print_string fmt m

  type t =
    | Admin [@name "admin"] [@printer field_name "admin"]
    | AssetId [@name "asset_id"] [@printer field_name "asset_id"]
    | AssignmentCount [@name "assignment_count"]
        [@printer field_name "assignment_count"]
    | CanceledAt [@name "canceled_at"] [@printer field_name "canceled_at"]
    | Comment [@name "comment"] [@printer field_name "comment"]
    | Contact [@name "contact"] [@printer field_name "contact"]
    | ContactEmail [@name "contact_email"] [@printer field_name "contact_email"]
    | Contacts [@name "contacts"] [@printer field_name "contacts"]
    | CreatedAt [@name "created_at"] [@printer field_name "created_at"]
    | CurrentPassword [@name "current_password"]
        [@printer field_name "current_password"]
    | Database [@name "database"] [@printer field_name "database"]
    | DatabaseLabel [@name "database_label"]
        [@printer field_name "database_label"]
    | DatabaseUrl [@name "database_url"] [@printer field_name "database_url"]
    | Date [@name "date"] [@printer field_name "date"]
    | DateTime [@name "date_time"] [@printer field_name "date_time"]
    | DefaultLanguage [@name "default_language"]
        [@printer field_name "default_language"]
    | Description [@name "description"] [@printer field_name "description"]
    | DirectRegistrationDisabled [@name "direct_registration_disabled"]
        [@printer field_name "direct_registration_disabled"]
    | Disabled [@name "disabled"] [@printer field_name "disabled"]
    | Duration [@name "duration"] [@printer field_name "duration"]
    | Email [@name "email"] [@printer field_name "email"]
    | EmailAddress [@name "email_address"] [@printer field_name "email_address"]
    | EmailAddressUnverified [@name "email_address_unverified"]
        [@printer field_name "email_address_unverified"]
    | EmailAddressVerified [@name "email_address_verified"]
        [@printer field_name "email_address_verified"]
    | EmailSuffix [@name "email_suffix"] [@printer field_name "email_suffix"]
    | Experiment [@name "experiment"] [@printer field_name "experiment"]
    | File [@name "file"] [@printer field_name "file"]
    | FileMimeType [@name "file_mime_type"]
        [@printer field_name "file_mime_type"]
    | Filename [@name "filename"] [@printer field_name "filename"]
    | Filesize [@name "filesize"] [@printer field_name "filesize"]
    | Firstname [@name "firstname"] [@printer field_name "firstname"]
    | Host [@name "host"] [@printer field_name "host"]
    | I18n [@name "i18n"] [@printer field_name "i18n"]
    | Icon [@name "icon"] [@printer field_name "icon"]
    | Id [@name "id"] [@printer field_name "id"]
    | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
        [@printer field_name "inactive_user_disable_after"]
    | InactiveUserWarning [@name "inactive_user_warning"]
        [@printer field_name "inactive_user_warning"]
    | Invitation [@name "invitation"] [@printer field_name "invitation"]
    | Invitations [@name "invitations"] [@printer field_name "invitations"]
    | Key [@name "key"] [@printer field_name "key"]
    | Language [@name "language"] [@printer field_name "language"]
    | LanguageDe [@name "language_de"] [@printer field_name "language_de"]
    | LanguageEn [@name "language_en"] [@printer field_name "language_en"]
    | Lastname [@name "lastname"] [@printer field_name "lastname"]
    | LogoType [@name "logo_type"] [@printer field_name "logo_type"]
    | MaxParticipants [@name "max_participants"]
        [@printer field_name "max_participants"]
    | MinParticipants [@name "min_participants"]
        [@printer field_name "min_participants"]
    | Name [@name "name"] [@printer field_name "name"]
    | NewPassword [@name "new_password"] [@printer field_name "new_password"]
    | Operator [@name "operator"] [@printer field_name "operator"]
    | Overbook [@name "overbook"] [@printer field_name "overbook"]
    | Page [@name "page"] [@printer field_name "page"]
    | Participant [@name "participant"] [@printer field_name "participant"]
    | ParticipantCount [@name "participant_count"]
        [@printer field_name "participant_count"]
    | Participants [@name "participants"] [@printer field_name "participants"]
    | Participated [@name "participated"] [@printer field_name "participated"]
    | PartnerLogos [@name "partner_logos"] [@printer field_name "partner_logos"]
    | Password [@name "password"] [@printer field_name "password"]
    | PasswordConfirmation [@name "password_confirmation"]
        [@printer field_name "password_confirmation"]
    | Paused [@name "paused"] [@printer field_name "paused"]
    | RecruitmentChannel [@name "recruitment_channel"]
        [@printer field_name "recruitment_channel"]
    | ResentAt [@name "resent_at"] [@printer field_name "resent_at"]
    | Role [@name "role"] [@printer field_name "role"]
    | Root [@name "root"] [@printer field_name "root"]
    | Session [@name "session"] [@printer field_name "session"]
    | Setting [@name "setting"] [@printer field_name "setting"]
    | ShowUp [@name "show_up"] [@printer field_name "show_up"]
    | SmtpAuthMethod [@name "smtp_auth_method"]
        [@printer field_name "smtp_auth_method"]
    | SmtpAuthServer [@name "smtp_auth_server"]
        [@printer field_name "smtp_auth_server"]
    | SmtpPassword [@name "smtp_password"] [@printer field_name "smtp_password"]
    | SmtpPort [@name "smtp_port"] [@printer field_name "smtp_port"]
    | SmtpProtocol [@name "smtp_protocol"] [@printer field_name "smtp_protocol"]
    | SmtpReadModel [@name "smtp_read_model"]
        [@printer field_name "smtp_read_model"]
    | SmtpUsername [@name "smtp_username"] [@printer field_name "smtp_username"]
    | SmtpWriteModel [@name "smtp_write_model"]
        [@printer field_name "smtp_write_model"]
    | Start [@name "start"] [@printer field_name "start"]
    | Styles [@name "styles"] [@printer field_name "styles"]
    | Tenant [@name "tenant"] [@printer field_name "tenant"]
    | TenantDisabledFlag [@name "tenant_disabled_flag"]
        [@printer field_name "tenant_disabled_flag"]
    | TenantId [@name "tenant_id"] [@printer field_name "tenant_id"]
    | TenantLogos [@name "tenant_logos"] [@printer field_name "tenant_logos"]
    | TenantMaintenanceFlag [@name "tenant_maintenance_flag"]
        [@printer field_name "tenant_maintenance_flag"]
    | TenantPool [@name "tenant_pool"] [@printer field_name "tenant_pool"]
    | TermsAccepted [@name "terms_accepted"]
        [@printer field_name "terms_accepted"]
    | TermsAndConditions [@name "terms_and_conditions"]
        [@printer field_name "terms_and_conditions"]
    | Time [@name "time"] [@printer field_name "time"]
    | TimeSpan [@name "timespan"] [@printer field_name "timespan"]
    | Title [@name "title"] [@printer field_name "title"]
    | Token [@name "token"] [@printer field_name "token"]
    | Translation [@name "translation"] [@printer field_name "translation"]
    | Url [@name "url"] [@printer field_name "url"]
    | User [@name "user"] [@printer field_name "user"]
    | Version [@name "version"] [@printer field_name "version"]
    | WaitingList [@name "waiting_list"] [@printer field_name "waiting_list"]
    | WaitingListDisabled [@name "waiting_list_disabled"]
        [@printer field_name "waiting_list_disabled"]
  [@@deriving eq, show { with_path = false }, yojson, variants, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let url_key m = m |> show |> Format.asprintf ":%s"
  let array_key m = m |> show |> Format.asprintf "%s[]"
end

(* TODO [aerben] make these general, compare what fields exist already, whenever
   pattern is "FIELD_ADJECTIVE", turn FIELD to Field.t and make it ADJECTIVE of
   Field.t *)
type error =
  | AlreadySignedUpForExperiment
  | Conformist of (Field.t * error) list
  | ConformistModuleErrorType
  | ContactSignupInvalidEmail
  | ContactUnconfirmed
  | Decode of Field.t
  | DecodeAction
  | Disabled of Field.t
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailDeleteAlreadyVerified
  | EmailMalformed
  | ExperimentSessionCountNotZero
  | HtmxVersionNotFound of string
  | Invalid of Field.t
  | LoginProvideDetails
  | MeantimeUpdate of Field.t
  | NegativeAmount
  | NoOptionSelected of Field.t
  | NotADatetime of (string * string)
  | NotANumber of string
  | NoTenantsRegistered
  | NotFound of Field.t
  | NotFoundList of Field.t * string list
  | NotEligible
  | NotHandled of string
  | NoValue
  | PasswordConfirmationDoesNotMatch
  | PasswordPolicy of string
  | PasswordResetFailMessage
  | PasswordResetInvalidData
  | PoolContextNotFound
  | RequestRequiredFields
  | Retrieve of Field.t
  | SessionInvalid
  | SessionTenantNotFound
  | Smaller of (Field.t * Field.t)
  | TerminatoryRootError
  | TerminatoryRootErrorTitle
  | TerminatoryTenantError
  | TerminatoryTenantErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TimeInPast
  | TimeSpanPositive
  | TokenAlreadyUsed
  | TokenInvalidFormat
  | Undefined of Field.t
  | WaitingListFlagsMutuallyExclusive
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants, sexp_of]

type warning = Warning of string
[@@deriving eq, show, yojson, variants, sexp_of]

type success =
  | AddedToWaitingList
  | AssignmentCreated
  | Canceled of Field.t
  | Created of Field.t
  | Deleted of Field.t
  | EmailConfirmationMessage
  | EmailVerified
  | FileDeleted
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | RemovedFromWaitingList
  | SentList of Field.t
  | SettingsUpdated
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
  | Back
  | Cancel of Field.t option
  | Choose of Field.t option
  | Create of Field.t option
  | Decline
  | Delete of Field.t option
  | Disable
  | Edit of Field.t option
  | Enable
  | Enroll
  | Login
  | More
  | RemoveFromWaitingList
  | Resend of Field.t option
  | Save of Field.t option
  | Send of Field.t option
  | SendResetLink
  | SignUp
  | Update of Field.t option
[@@deriving eq, show, yojson, variants, sexp_of]

let to_conformist_error error_list =
  CCList.map (fun (name, _, msg) -> name |> Field.read, msg) error_list
  |> conformist
;;

let add_field_query_params path params =
  CCList.map (CCPair.map_fst Field.show) params
  |> Uri.add_query_params' (Uri.of_string path)
  |> Uri.to_string
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
