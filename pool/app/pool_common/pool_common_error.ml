let handle_conformist_error (err : Conformist.error list) =
  String.concat
    "\n"
    (List.map (fun (m, _, k) -> Format.asprintf "%s: %s" m k) err)
;;

type conformist_error = Conformist.error

let equal_conformist_error
    ((f1, _, _) : conformist_error)
    ((f2, _, _) : conformist_error)
  =
  String.equal f1 f2
;;

let pp_conformist_error f ((m, _, _) : conformist_error) =
  Format.pp_print_string f m
;;

type field =
  | DatabaseUrl
  | DatabaseLabel
  | EmailAddress
  | EmailSuffix
  | FileMimeType
  | Language
  | Password
  | Firstname
  | Lastname
  | RecruitmentChannel
  | SmtpAuthServer
  | SmtpPort
  | SmtpUsername
  | SmtpPassword
  | SmtpAuthMethod
  | SmtpProtocol
  | Title
  | Description
  | Url
  | Styles
  | Icon
  | LogoType
  | TenantMaintenanceFlag
  | TenantDisabledFlag
[@@deriving eq, show]

type not_found =
  | User
  | Participant
  | Admin
  | Tenant
  | Host
  | Email
  | Token
[@@deriving eq, show]

type t =
  | PasswordPolicy of string
  | PasswordResetMessage
  | PasswordResetInvalidData
  | PasswordResetFinish
  | ParticipantUnconfirmed
  | ParticipantSignupInvalidEmail
  | TermsAndConditionsNotAccepted
  | TenantSessionNotFound
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailMalformed
  | EmailAlreadyInUse
  | LoginProvideDetails
  | NotFound of not_found
  | Undefined of field
  | Invalid of field
  | Conformist of conformist_error list
  | RequestRequiredFields
  | TokenInvalidFormat
  | NoTenantsRegistered
[@@deriving eq, show, variants]

let field_message prefix field suffix =
  let field_name =
    match field with
    | DatabaseUrl -> "database url"
    | DatabaseLabel -> "database label"
    | EmailAddress -> "email address"
    | EmailSuffix -> "email suffix"
    | FileMimeType -> "mime type"
    | Language -> "language"
    | Password -> "password"
    | Firstname -> "firstname"
    | Lastname -> "lastname"
    | RecruitmentChannel -> "recruitment channel"
    | SmtpAuthServer -> "smtp authentication server"
    | SmtpPort -> "smtp port"
    | SmtpUsername -> "smtp username"
    | SmtpPassword -> "smtp password"
    | SmtpAuthMethod -> "smtp authentication method"
    | SmtpProtocol -> "smtp protocol"
    | Title -> "title"
    | Description -> "description"
    | Url -> "url"
    | Styles -> "styles"
    | Icon -> "icon"
    | LogoType -> "logo type"
    | TenantMaintenanceFlag -> "maintenance flag"
    | TenantDisabledFlag -> "disabled flag"
  in
  Format.asprintf "%s %s %s" prefix field_name suffix |> String.trim
;;

let not_found_message field =
  Format.asprintf "%s not found!"
  @@
  match field with
  | User -> "User"
  | Participant -> "Participant"
  | Admin -> "Admin"
  | Tenant -> "Tenant"
  | Host -> "Host"
  | Email -> "Email address"
  | Token -> "Token"
;;

let message = function
  | PasswordPolicy msg ->
    Format.asprintf "Password doesn't match the required policy! %s" msg
  | PasswordResetMessage ->
    "You will receive an email with a link to reset your password if an \
     account with the provided email is existing."
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PasswordResetFinish -> "Password reset, you can now log in."
  | ParticipantUnconfirmed -> "Participant isn't confirmed!"
  | ParticipantSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TenantSessionNotFound ->
    "Something on our side went wrong, please try again later or on multi \
     occurrences please contact the Administrator."
  | EmailAddressMissingOperator -> "Please provide operator email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailMalformed -> "Malformed email"
  | EmailAlreadyInUse -> "Email address is already in use."
  | LoginProvideDetails -> "Please provide email and password"
  | NotFound field -> not_found_message field
  | Undefined field -> field_message "Undefined" field ""
  | Invalid field -> field_message "Invalid" field "provided!"
  | Conformist err -> handle_conformist_error err
  | RequestRequiredFields -> "Please provide necessary fields"
  | TokenInvalidFormat -> "Invalid Token Format!"
  | NoTenantsRegistered -> "There are no tenants registered in root database!"
;;

let to_string = Lwt.return message

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Password
;;
