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
  | Token
  | Url
  | User
[@@deriving eq, show, yojson, variants]

type error =
  | AdminAndParticipantSimul
  | AlreadyExists of field
  | CantPromote
  | Conformist of ConformistError.t list
  | DecodeAction
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailMalformed
  | ErrorList of error list
  | Invalid of field
  | LoginProvideDetails
  | MeantimeUpdate of field
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

type warning = WarningList of warning list
[@@deriving eq, show, yojson, variants]

type success =
  | Created of field
  | EmailVerified
  | EmailConfirmationMessage
  | FileDeleted
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | SettingsUpdated
  | SuccessList of success list
  | TenantUpdateDatabase
  | TenantUpdateDetails
  | Updated of field
[@@deriving eq, show, yojson, variants]

type info =
  | InfoList of info list
  | TryPromoteOperator
[@@deriving eq, show, yojson, variants]

type t =
  | ErrorM of error
  | InfoM of info
  | MessageList of t list
  | Message of string
  | PageNotFoundMessage
  | SuccessM of success
  | WarningM of warning
[@@deriving eq, show, yojson, variants]

type product =
  { error : error list
  ; warning : warning list
  ; success : success list
  ; info : info list
  }
[@@deriving eq, show, yojson]

let empty_product = { error = []; warning = []; success = []; info = [] }

let separate msgs =
  let rec go ({ error; warning; success; info } as p) msg =
    match msg with
    | ErrorM m -> { p with error = m :: error }
    | InfoM m -> { p with info = m :: info }
    | WarningM m -> { p with warning = m :: warning }
    | SuccessM m -> { p with success = m :: success }
    | MessageList m ->
      let foo = CCList.map (go empty_product) m in
      CCList.fold_left
        (fun acc cur ->
          { error = acc.error @ cur.error
          ; warning = acc.warning @ cur.warning
          ; success = acc.success @ cur.success
          ; info = acc.info @ cur.info
          })
        empty_product
        foo
    | Message _ | PageNotFoundMessage -> p
  in
  go empty_product msgs
;;

let field_message prefix field suffix =
  Format.asprintf "%s %s %s" prefix field suffix
  |> CCString.trim
  |> CCString.capitalize_ascii
;;

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Password
;;

let list_msg_to_string fn list = list |> CCList.map fn |> CCString.concat " "

module I18n = struct
  module EmailConfirmation = struct
    let title = "Email confirmation"
    let note = "Please check your emails and confirm your address first."
  end
end
