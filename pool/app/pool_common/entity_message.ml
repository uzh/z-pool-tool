type field =
  | Admin
  | ContactEmail
  | CurrentPassword
  | Database
  | DatabaseLabel
  | DatabaseUrl
  | DefaultLanguage
  | Description
  | Email
  | EmailAddress
  | EmailSuffix
  | EmailAddressUnverified
  | EmailAddressVerified
  | File
  | FileMimeType
  | Filename
  | Filesize
  | Firstname
  | Host
  | I18n
  | Icon
  | InactiveUserDisableAfter
  | InactiveUserWarning
  | Key
  | Language
  | LanguageDe
  | LanguageEn
  | Lastname
  | LogoType
  | NewPassword
  | Operator
  | Page
  | Participant
  | Password
  | PasswordConfirmation
  | Paused
  | RecruitmentChannel
  | Role
  | Root
  | Setting
  | SmtpAuthMethod
  | SmtpAuthServer
  | SmtpPassword
  | SmtpPort
  | SmtpProtocol
  | SmtpReadModel
  | SmtpWriteModel
  | SmtpUsername
  | Styles
  | Tenant
  | TenantDisabledFlag
  | TenantLogos
  | TenantMaintenanceFlag
  | TenantPool
  | TermsAndConditions
  | TimeSpan
  | Title
  | Translation
  | Token
  | Url
  | User
[@@deriving eq, show { with_path = false }, yojson, variants]

type error =
  | Conformist of error list
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

let to_coformist_error error_list =
  CCList.map (fun (_, _, msg) -> msg) error_list |> conformist
;;

let field_name field = field |> show_field |> CCString.lowercase_ascii

let add_field_query_params path params =
  CCList.map (CCPair.map_fst field_name) params
  |> Uri.add_query_params' (Uri.of_string path)
  |> Uri.to_string
;;
