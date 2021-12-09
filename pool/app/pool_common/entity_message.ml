module ConformistError = struct
  type t = string * string list * string [@@deriving eq, show, yojson]

  let to_string err =
    CCString.concat
      "\n"
      (List.map (fun (m, _, k) -> Format.asprintf "%s: %s" m k) err)
  ;;
end

type field =
  | Admin
  | ContactEmail
  | DatabaseLabel
  | DatabaseUrl
  | Description
  | Email
  | EmailAddress
  | EmailSuffix
  | FileMimeType
  | Filename
  | Filesize
  | Firstname
  | Host
  | Icon
  | InactiveUserDisableAfter
  | InactiveUserWarning
  | Language
  | Lastname
  | LogoType
  | Operator
  | Page
  | Participant
  | Password
  | RecruitmentChannel
  | Role
  | Root
  | Setting
  | SmtpAuthMethod
  | SmtpAuthServer
  | SmtpPassword
  | SmtpPort
  | SmtpProtocol
  | SmtpUsername
  | Styles
  | Tenant
  | TenantDisabledFlag
  | TenantLogos
  | TenantMaintenanceFlag
  | TermsAndConditions
  | TimeSpan
  | Title
  | Token
  | Url
  | User
[@@deriving eq, show, yojson, variants]

type error =
  | Conformist of ConformistError.t list
  | DecodeAction
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailMalformed
  | Invalid of field
  | LoginProvideDetails
  | NoOptionSelected of field
  | NoTenantsRegistered
  | NotFound of field
  | ParticipantSignupInvalidEmail
  | ParticipantUnconfirmed
  | PasswordPolicy of string
  | PasswordResetInvalidData
  | PasswordResetFailMessage
  | RequestRequiredFields
  | Retrieve of field
  | SessionInvalid
  | SessionTenantNotFound
  | TerminatoryTenantError
  | TerminatoryRootError
  | TerminatoryTenantErrorTitle
  | TerminatoryRootErrorTitle
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

module I18n = struct
  module EmailConfirmation = struct
    let title = "Email confirmation"
    let note = "Please check your emails and confirm your address first."
  end
end
