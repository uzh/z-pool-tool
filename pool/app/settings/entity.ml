module Message = Pool_common.Message

module Day = struct
  type t = int [@@deriving eq, show, yojson]

  let to_timespan t = t * 24 * 60 * 60 |> Ptime.Span.of_int_s
end

module Week = struct
  type t = int [@@deriving eq, show, yojson]

  let to_timespan t = t * 7 * 24 * 60 * 60 |> Ptime.Span.of_int_s
end

module ContactEmail = struct
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  (* TODO: email address validation *)
  let create email =
    if CCString.length email <= 0
    then Error Message.(Invalid EmailAddress)
    else Ok email
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value Message.EmailAddress
  ;;
end

module EmailSuffix = struct
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  let create suffix =
    if CCString.length suffix <= 0
    then Error Pool_common.Message.(Invalid EmailSuffix)
    else Ok suffix
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value Message.EmailSuffix
  ;;
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Week.t [@@deriving eq, show, yojson]

    let create week =
      let open CCResult.Infix in
      week
      |> CCInt.of_string
      |> CCOption.to_result Pool_common.Message.(Invalid TimeSpan)
      >>= fun week ->
      if week < 0 then Error Pool_common.Message.TimeSpanPositive else Ok week
    ;;

    let value m = m
    let to_timespan = Week.to_timespan

    let schema () =
      Pool_common.Utils.schema_decoder
        create
        CCInt.to_string
        Message.InactiveUserDisableAfter
    ;;
  end

  module Warning = struct
    type t = Day.t [@@deriving eq, show, yojson]

    let create day =
      let open CCResult.Infix in
      day
      |> CCInt.of_string
      |> CCOption.to_result Pool_common.Message.(Invalid TimeSpan)
      >>= fun day ->
      if day < 0 then Error Pool_common.Message.TimeSpanPositive else Ok day
    ;;

    let value m = m
    let to_timespan = Day.to_timespan

    let schema () =
      Pool_common.Utils.schema_decoder
        create
        CCInt.to_string
        Message.InactiveUserWarning
    ;;
  end
end

module TermsAndConditions = struct
  module Terms = struct
    type t = string [@@deriving eq, show, yojson]

    let create terms =
      if CCString.length terms > 0
      then Ok terms
      else Error Pool_common.Message.(Invalid TermsAndConditions)
    ;;

    let value m = m
  end

  type t = Pool_common.Language.t * Terms.t [@@deriving eq, show, yojson]

  let create language content =
    let open CCResult in
    let* language = Pool_common.Language.of_string language in
    let* content = Terms.create content in
    Ok (language, content)
  ;;

  let value m = m
end

module Value = struct
  type tenant_languages = Pool_common.Language.t list
  [@@deriving eq, show, yojson]

  type tenant_email_suffixes = EmailSuffix.t list [@@deriving eq, show, yojson]
  type tenant_contact_email = ContactEmail.t [@@deriving eq, show, yojson]

  type inactive_user_disable_after = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type inactive_user_warning = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type terms_and_conditions = TermsAndConditions.t list
  [@@deriving eq, show, yojson]

  type t =
    | TenantLanguages of tenant_languages
    | TenantEmailSuffixes of tenant_email_suffixes
    | TenantContactEmail of tenant_contact_email
    | InactiveUserDisableAfter of inactive_user_disable_after
    | InactiveUserWarning of inactive_user_warning
    | TermsAndConditions of terms_and_conditions
  [@@deriving eq, show, yojson, variants]
end

type setting_key =
  | Languages [@name "languages"]
  | EmailSuffixes [@name "email_suffixes"]
  | ContactEmail [@name "contact_email"]
  | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
  | InactiveUserWarning [@name "inactive_user_warning"]
  | TermsAndConditions [@name "terms_and_conditions"]
[@@deriving eq, show, yojson]

type t =
  { value : Value.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

module Write = struct
  type t = { value : Value.t }
end

let action_of_param = function
  | "update_tenant_languages" -> Ok `UpdateTenantLanguages
  | "update_tenant_emailsuffix" -> Ok `UpdateTenantEmailSuffixes
  | "create_tenant_emailsuffix" -> Ok `CreateTenantEmailSuffix
  | "delete_tenant_emailsuffix" -> Ok `DeleteTenantEmailSuffix
  | "update_tenant_contact_email" -> Ok `UpdateTenantContactEmail
  | "update_inactive_user_disable_after" -> Ok `UpdateInactiveUserDisableAfter
  | "update_inactive_user_warning" -> Ok `UpdateInactiveUserWarning
  | "update_terms_and_conditions" -> Ok `UpdateTermsAndConditions
  | _ -> Error Pool_common.Message.DecodeAction
;;

let stringify_action = function
  | `UpdateTenantLanguages -> "update_tenant_languages"
  | `UpdateTenantEmailSuffixes -> "update_tenant_emailsuffix"
  | `CreateTenantEmailSuffix -> "create_tenant_emailsuffix"
  | `UpdateTenantContactEmail -> "update_tenant_contact_email"
  | `DeleteTenantEmailSuffix -> "delete_tenant_emailsuffix"
  | `UpdateInactiveUserDisableAfter -> "update_inactive_user_disable_after"
  | `UpdateInactiveUserWarning -> "update_inactive_user_warning"
  | `UpdateTermsAndConditions -> "update_terms_and_conditions"
;;
