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
  | Csrf
  | DatabaseLabel
  | DatabaseUrl
  | Description
  | EmailAddress
  | EmailSuffix
  | FileMimeType
  | Firstname
  | Icon
  | Language
  | Lastname
  | LogoType
  | Password
  | RecruitmentChannel
  | Role
  | SmtpAuthMethod
  | SmtpAuthServer
  | SmtpPassword
  | SmtpPort
  | SmtpProtocol
  | SmtpUsername
  | Styles
  | TenantDisabledFlag
  | TenantMaintenanceFlag
  | Title
  | Url
[@@deriving eq, show]

type not_found =
  | Admin
  | Email
  | Host
  | Page
  | Participant
  | Tenant
  | Token
  | User
[@@deriving eq, show]

type created = Tenant [@@deriving eq, show]

type t =
  | Conformist of conformist_error list
  | Created of created
  | EmailAddressMissingOperator
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailMalformed
  | Invalid of field
  | InvalidSession
  | LoginProvideDetails
  | NoTenantsRegistered
  | NotFound of not_found
  | PageNotFoundMessage
  | ParticipantSignupInvalidEmail
  | ParticipantUnconfirmed
  | PasswordPolicy of string
  | PasswordResetFinish
  | PasswordResetInvalidData
  | PasswordResetMessage
  | RequestRequiredFields
  | TenantSessionNotFound
  | TermsAndConditionsNotAccepted
  | TokenInvalidFormat
  | Undefined of field
[@@deriving eq, show, variants]

let field_message prefix field suffix =
  let field_name =
    match field with
    | Csrf -> "session"
    | DatabaseLabel -> "database label"
    | DatabaseUrl -> "database url"
    | Description -> "description"
    | EmailAddress -> "email address"
    | EmailSuffix -> "email suffix"
    | FileMimeType -> "mime type"
    | Firstname -> "firstname"
    | Icon -> "icon"
    | Language -> "language"
    | Lastname -> "lastname"
    | LogoType -> "logo type"
    | Password -> "password"
    | RecruitmentChannel -> "recruitment channel"
    | Role -> "role"
    | SmtpAuthMethod -> "smtp authentication method"
    | SmtpAuthServer -> "smtp authentication server"
    | SmtpPassword -> "smtp password"
    | SmtpPort -> "smtp port"
    | SmtpProtocol -> "smtp protocol"
    | SmtpUsername -> "smtp username"
    | Styles -> "styles"
    | TenantDisabledFlag -> "disabled flag"
    | TenantMaintenanceFlag -> "maintenance flag"
    | Title -> "title"
    | Url -> "url"
  in
  Format.asprintf "%s %s %s" prefix field_name suffix |> String.trim
;;

let not_found_message field =
  Format.asprintf "%s not found!"
  @@
  match field with
  | Admin -> "Admin"
  | Email -> "Email address"
  | Host -> "Host"
  | Page -> "Page"
  | Participant -> "Participant"
  | Tenant -> "Tenant"
  | Token -> "Token"
  | User -> "User"
;;

let created_message (field : created) =
  Format.asprintf "%s was successfully created."
  @@
  match field with
  | Tenant -> "Tenant"
;;

let message = function
  | Conformist err -> handle_conformist_error err
  | Created field -> created_message field
  | EmailAddressMissingOperator -> "Please provide operator email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailAlreadyInUse -> "Email address is already in use."
  | EmailMalformed -> "Malformed email"
  | Invalid field -> field_message "Invalid" field "provided!"
  | InvalidSession -> "Invalid session, please login."
  | LoginProvideDetails -> "Please provide email and password"
  | NoTenantsRegistered -> "There are no tenants registered in root database!"
  | NotFound field -> not_found_message field
  | PageNotFoundMessage -> "The requested page could not be found."
  | ParticipantSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | ParticipantUnconfirmed -> "Participant isn't confirmed!"
  | PasswordPolicy msg ->
    Format.asprintf "Password doesn't match the required policy! %s" msg
  | PasswordResetFinish -> "Password reset, you can now log in."
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PasswordResetMessage ->
    "You will receive an email with a link to reset your password if an \
     account with the provided email is existing."
  | RequestRequiredFields -> "Please provide necessary fields"
  | TenantSessionNotFound ->
    "Something on our side went wrong, please try again later or on multi \
     occurrences please contact the Administrator."
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TokenInvalidFormat -> "Invalid Token Format!"
  | Undefined field -> field_message "Undefined" field ""
;;

let to_string = Lwt.return message

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Password
;;

module I18n = struct
  module EmailConfirmation = struct
    let title = "Email confirmation"
    let note = "Please check your emails and confirm your address first."

    let message =
      "Successfully created. An email has been sent to your email address for \
       verification."
    ;;
  end

  module RootTenant = struct
    let message_update_detail = "Tenant was successfully updated."

    let message_update_database =
      "Database information was successfully updated."
    ;;
  end
end
