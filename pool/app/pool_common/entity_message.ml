let pp m fmt _ = Format.pp_print_string fmt m

type field =
  | Admin [@name "admin"] [@printer pp "admin"]
  | ContactEmail [@name "contact_email"] [@printer pp "contact_email"]
  | CurrentPassword [@name "current_password"] [@printer pp "current_password"]
  | Database [@name "database"] [@printer pp "database"]
  | DatabaseLabel [@name "database_label"] [@printer pp "database_label"]
  | DatabaseUrl [@name "database_url"] [@printer pp "database_url"]
  | DefaultLanguage [@name "default_language"] [@printer pp "default_language"]
  | Description [@name "description"] [@printer pp "description"]
  | Email [@name "email"] [@printer pp "email"]
  | EmailAddress [@name "email_address"] [@printer pp "email_address"]
  | EmailSuffix [@name "email_suffix"] [@printer pp "email_suffix"]
  | EmailAddressUnverified [@name "email_address_unverified"]
      [@printer pp "email_address_unverified"]
  | EmailAddressVerified [@name "email_address_verified"]
      [@printer pp "email_address_verified"]
  | File [@name "file"] [@printer pp "file"]
  | FileMimeType [@name "file_mime_type"] [@printer pp "file_mime_type"]
  | Filename [@name "filename"] [@printer pp "filename"]
  | Filesize [@name "filesize"] [@printer pp "filesize"]
  | Firstname [@name "firstname"] [@printer pp "firstname"]
  | Host [@name "host"] [@printer pp "host"]
  | I18n [@name "i18n"] [@printer pp "i18n"]
  | Icon [@name "icon"] [@printer pp "icon"]
  | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
      [@printer pp "inactive_user_disable_after"]
  | InactiveUserWarning [@name "inactive_user_warning"]
      [@printer pp "inactive_user_warning"]
  | Key [@name "key"] [@printer pp "key"]
  | Language [@name "language"] [@printer pp "language"]
  | LanguageDe [@name "language_de"] [@printer pp "language_de"]
  | LanguageEn [@name "language_en"] [@printer pp "language_en"]
  | Lastname [@name "lastname"] [@printer pp "lastname"]
  | LogoType [@name "logo_type"] [@printer pp "logo_type"]
  | NewPassword [@name "new_password"] [@printer pp "new_password"]
  | Operator [@name "operator"] [@printer pp "operator"]
  | Page [@name "page"] [@printer pp "page"]
  | Participant [@name "participant"] [@printer pp "participant"]
  | PartnerLogos [@name "partner_logos"] [@printer pp "partner_logos"]
  | Password [@name "password"] [@printer pp "password"]
  | PasswordConfirmation [@name "password_confirmation"]
      [@printer pp "password_confirmation"]
  | Paused [@name "paused"] [@printer pp "paused"]
  | RecruitmentChannel [@name "recruitment_channel"]
      [@printer pp "recruitment_channel"]
  | Role [@name "role"] [@printer pp "role"]
  | Root [@name "root"] [@printer pp "root"]
  | Setting [@name "setting"] [@printer pp "setting"]
  | SmtpAuthMethod [@name "smtp_auth_method"] [@printer pp "smtp_auth_method"]
  | SmtpAuthServer [@name "smtp_auth_server"] [@printer pp "smtp_auth_server"]
  | SmtpPassword [@name "smtp_password"] [@printer pp "smtp_password"]
  | SmtpPort [@name "smtp_port"] [@printer pp "smtp_port"]
  | SmtpProtocol [@name "smtp_protocol"] [@printer pp "smtp_protocol"]
  | SmtpReadModel [@name "smtp_read_model"] [@printer pp "smtp_read_model"]
  | SmtpWriteModel [@name "smtp_write_model"] [@printer pp "smtp_write_model"]
  | SmtpUsername [@name "smtp_username"] [@printer pp "smtp_username"]
  | Styles [@name "styles"] [@printer pp "styles"]
  | Tenant [@name "tenant"] [@printer pp "tenant"]
  | TenantDisabledFlag [@name "tenant_disabled_flag"]
      [@printer pp "tenant_disabled_flag"]
  | TenantLogos [@name "tenant_logos"] [@printer pp "tenant_logos"]
  | TenantMaintenanceFlag [@name "tenant_maintenance_flag"]
      [@printer pp "tenant_maintenance_flag"]
  | TenantPool [@name "tenant_pool"] [@printer pp "tenant_pool"]
  | TermsAndConditions [@name "terms_and_conditions"]
      [@printer pp "terms_and_conditions"]
  | TimeSpan [@name "timespan"] [@printer pp "timespan"]
  | Title [@name "title"] [@printer pp "title"]
  | Translation [@name "translation"] [@printer pp "translation"]
  | Token [@name "token"] [@printer pp "token"]
  | Url [@name "url"] [@printer pp "url"]
  | User [@name "user"] [@printer pp "user"]
[@@deriving eq, show { with_path = false }, yojson, variants]

type error =
  | Conformist of (field * error) list
  | ConformistModuleErrorType
  | DecodeAction
  | Decode of field
  | Disabled of field
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailMalformed
  | HtmxVersionNotFound of string
  | Invalid of field
  | LoginProvideDetails
  | MeantimeUpdate of field
  | NoOptionSelected of field
  | NotANumber of string
  | NoTenantsRegistered
  | NotFound of field
  | NotHandled of string
  | NoValue
  | ParticipantSignupInvalidEmail
  | ParticipantUnconfirmed
  | PasswordPolicy of string
  | PasswordResetInvalidData
  | PasswordResetFailMessage
  | RequestRequiredFields
  | Retrieve of field
  | SessionInvalid
  | SessionTenantNotFound
  | PoolContextNotFound
  | TerminatoryTenantError
  | TerminatoryRootError
  | TerminatoryTenantErrorTitle
  | TerminatoryRootErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TimeSpanPositive
  | TokenInvalidFormat
  | TokenAlreadyUsed
  | Undefined of field
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants]

type warning = Warning of string [@@deriving eq, show, yojson, variants]

type success =
  | Created of field
  | EmailVerified
  | EmailConfirmationMessage
  | FileDeleted
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | SettingsUpdated
  | TenantUpdateDatabase
  | TenantUpdateDetails
  | Updated of field
[@@deriving eq, show, yojson, variants]

type info = Info of string [@@deriving eq, show, yojson, variants]

type t =
  | Message of string
  | PageNotFoundMessage
[@@deriving eq, show, yojson, variants]

let field_message prefix field suffix =
  Format.asprintf "%s %s %s" prefix field suffix
  |> CCString.trim
  |> CCString.capitalize_ascii
;;

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Password
;;

type control =
  | Accept of field option
  | Add of field option
  | Back
  | Choose of field option
  | Create of field option
  | Delete of field option
  | Decline
  | Disable
  | Edit of field option
  | Enable
  | Login
  | Save of field option
  | SendResetLink
  | SignUp
  | Update of field option
[@@deriving eq, show, yojson, variants]

let read_to_field m =
  m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> field_of_yojson
;;

let to_coformist_error error_list =
  CCList.map (fun (name, _, msg) -> name |> read_to_field, msg) error_list
  |> conformist
;;

let add_field_query_params path params =
  CCList.map (CCPair.map_fst show_field) params
  |> Uri.add_query_params' (Uri.of_string path)
  |> Uri.to_string
;;
