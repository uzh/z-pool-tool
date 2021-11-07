module Day = struct
  type t = int [@@deriving eq, show, yojson]

  let to_timespan t = t * 24 * 60 * 60 |> Ptime.Span.of_int_s
end

module Week = struct
  type t = int [@@deriving eq, show, yojson]

  let to_timespan t = t * 7 * 24 * 60 * 60 |> Ptime.Span.of_int_s
end

module Language = struct
  type t =
    | En [@name "EN"]
    | De [@name "DE"]
  [@@deriving eq, show, yojson]

  let code = function
    | En -> "EN"
    | De -> "DE"
  ;;

  let of_string = function
    | "EN" -> Ok En
    | "DE" -> Ok De
    | _ -> Error "Invalid Language privided"
  ;;

  let t =
    Caqti_type.(
      custom ~encode:(fun m -> m |> code |> Result.ok) ~decode:of_string string)
  ;;

  let label country_code = country_code |> code |> Utils.Countries.find

  let schema () =
    Conformist.custom
      (Utils.schema_decoder of_string "default language")
      (fun l -> [ code l ])
      "default_language"
  ;;

  let all () = [ En; De ]
  let all_codes () = [ En; De ] |> CCList.map code
end

module ContactEmail = struct
  (* TODO [timhub] : Use Common User Email -> Dependency cycle *)
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  let create email =
    if CCString.length email <= 0
    then Error "invalid email address!"
    else Ok email
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "contact email")
      CCList.pure
      "contact_email"
  ;;
end

module EmailSuffix = struct
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  let create suffix =
    if CCString.length suffix <= 0
    then Error "Invalid email suffix!"
    else Ok suffix
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "email suffix")
      CCList.pure
      "email_suffix"
  ;;
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Week.t [@@deriving eq, show, yojson]

    let create week =
      let open CCResult.Infix in
      week
      |> CCInt.of_string
      |> CCOpt.to_result "Invalid time span provided"
      >>= fun week ->
      if week < 0 then Error "Time span must be positive" else Ok week
    ;;

    let value m = m
    let to_timespan = Week.to_timespan

    let schema () =
      Conformist.custom
        (Utils.schema_decoder create "disable inactive user after")
        (fun l -> l |> CCInt.to_string |> CCList.pure)
        "inactive_user_disable_after"
    ;;
  end

  module Warning = struct
    type t = Day.t [@@deriving eq, show, yojson]

    let create day =
      let open CCResult.Infix in
      day
      |> CCInt.of_string
      |> CCOpt.to_result "Invalid time span provided"
      >>= fun day ->
      if day < 0 then Error "Time span must be positive" else Ok day
    ;;

    let value m = m
    let to_timespan = Day.to_timespan

    let schema () =
      Conformist.custom
        (Utils.schema_decoder create "disable inactive user warning")
        (fun l -> l |> CCInt.to_string |> CCList.pure)
        "inactive_user_warning"
    ;;
  end
end

module TermsAndConditions = struct
  type t = string [@@deriving eq, show, yojson]

  let create terms =
    if CCString.length terms > 0
    then Ok terms
    else Error "Invalid terms provided"
  ;;

  let value m = m

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "terms and conditions")
      CCList.pure
      "terms_and_conditions"
  ;;
end

module Value = struct
  type tenant_languages = Language.t list [@@deriving eq, show, yojson]
  type tenant_email_suffixes = EmailSuffix.t list [@@deriving eq, show, yojson]
  type tenant_contact_email = ContactEmail.t [@@deriving eq, show, yojson]

  type inactive_user_disable_after = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type inactive_user_warning = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type terms_and_conditions = TermsAndConditions.t [@@deriving eq, show, yojson]

  type t =
    | TenantLanguages of tenant_languages
    | TenantEmailSuffixes of tenant_email_suffixes
    | TenantContactEmail of tenant_contact_email
    | InactiveUserDisableAfter of inactive_user_disable_after
    | InactiveUserWarning of inactive_user_warning
    | TermsAndConditions of terms_and_conditions
  [@@deriving eq, show, yojson]
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
