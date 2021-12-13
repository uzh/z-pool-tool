open Entity_message

let field_to_string = function
  | Admin -> "admin"
  | ContactEmail -> "contact email address"
  | DatabaseLabel -> "database label"
  | DatabaseUrl -> "database url"
  | Description -> "description"
  | Email -> "email address"
  | EmailAddress -> "email address"
  | EmailSuffix -> "email suffix"
  | FileMimeType -> "mime type"
  | Filename -> "filename"
  | Filesize -> "filesize"
  | Firstname -> "firstname"
  | Host -> "host"
  | Icon -> "icon"
  | InactiveUserDisableAfter -> "disable inactive user after"
  | InactiveUserWarning -> "warn inactive user"
  | Language -> "language"
  | Lastname -> "lastname"
  | LogoType -> "logo type"
  | Operator -> "operator"
  | Page -> "page"
  | Participant -> "participant"
  | Password -> "password"
  | Paused -> "paused"
  | RecruitmentChannel -> "recruitment channel"
  | Role -> "role"
  | Root -> "root"
  | Setting -> "setting"
  | SmtpAuthMethod -> "smtp authentication method"
  | SmtpAuthServer -> "smtp authentication server"
  | SmtpPassword -> "smtp password"
  | SmtpPort -> "smtp port"
  | SmtpProtocol -> "smtp protocol"
  | SmtpUsername -> "smtp username"
  | Styles -> "styles"
  | Tenant -> "tenant"
  | TenantDisabledFlag -> "disabled flag"
  | TenantLogos -> "tenant logos"
  | TenantMaintenanceFlag -> "maintenance flag"
  | TenantPool -> "Tenant pool"
  | TermsAndConditions -> "terms and conditions"
  | TimeSpan -> "time span"
  | Title -> "title"
  | Token -> "token"
  | Url -> "url"
  | User -> "user"
;;

let info_to_string : info -> string = function
  | Info string -> string
;;

let success_to_string : success -> string = function
  | Created field ->
    field_message "" (field_to_string field) "was successfully created."
  | EmailVerified -> "Email successfully verified."
  | EmailConfirmationMessage ->
    "Successfully created. An email has been sent to your email address for \
     verification."
  | FileDeleted -> "File was successfully deleted."
  | PasswordChanged -> "Password successfully changed."
  | PasswordReset -> "Password reset, you can now log in."
  | PasswordResetSuccessMessage ->
    "You will receive an email with a link to reset your password if an \
     account with the provided email is existing."
  | SettingsUpdated -> "Settings were updated successfully."
  | TenantUpdateDatabase -> "Database information was successfully updated."
  | TenantUpdateDetails -> "Tenant was successfully updated."
  | Updated field ->
    field_message "" (field_to_string field) "was successfully updated."
;;

let warning_to_string : warning -> string = function
  | Warning string -> string
;;

let error_to_string = function
  | Conformist err -> ConformistError.to_string err
  | DecodeAction -> "Cannot decode action."
  | EmailAddressMissingOperator -> "Please provide operator email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailAlreadyInUse -> "Email address is already in use."
  | EmailMalformed -> "Malformed email"
  | Invalid field -> field_message "Invalid" (field_to_string field) "provided!"
  | LoginProvideDetails -> "Please provide email and password"
  | MeantimeUpdate field ->
    field_message "" (field_to_string field) "was updated in the meantime!"
  | NoOptionSelected field ->
    field_message "Please select at least one" (field_to_string field) "."
  | NoTenantsRegistered -> "There are no tenants registered in root database!"
  | NotFound field -> field_message "" (field_to_string field) "not found!"
  | ParticipantSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | ParticipantUnconfirmed -> "Participant isn't confirmed!"
  | PasswordPolicy msg ->
    Format.asprintf "Password doesn't match the required policy! %s" msg
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PasswordResetFailMessage ->
    "You will receive an email with a link to reset your password if an \
     account with the provided email is existing."
  | RequestRequiredFields -> "Please provide necessary fields"
  | Retrieve field -> field_message "Cannot retrieve" (field_to_string field) ""
  | SessionInvalid -> "Invalid session, please login."
  | SessionTenantNotFound ->
    "Something on our side went wrong, please try again later or on multi \
     occurrences please contact the Administrator."
  | TerminatoryTenantError | TerminatoryRootError -> "Please try again later."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "An error occurred"
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TimeSpanPositive -> "Time span must be positive!"
  | TokenInvalidFormat -> "Invalid Token Format!"
  | TokenAlreadyUsed -> "The token was already used."
  | Undefined field -> field_message "Undefined" (field_to_string field) ""
  | WriteOnlyModel -> "Write only model!"
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "The requested page could not be found."
;;
