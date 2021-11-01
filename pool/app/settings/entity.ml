module Day = struct
  type t = int
end

module Week = struct
  type t = int
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
  type t = string [@@deriving eq, show]

  let value m = m

  let create email =
    if CCString.length email <= 0
    then Error "invalid email address!"
    else Ok email
  ;;
end

module TenantLanguages = struct
  module Values = struct
    type t = Language.t list [@@deriving eq, show, yojson]

    let to_string m = m |> to_yojson |> Yojson.Safe.to_string
    let of_string m = m |> Yojson.Safe.from_string |> of_yojson
    let value m = m
  end

  let create languages =
    if CCList.length languages <= 0
    then Error "Select at least one language"
    else CCList.map Language.of_string languages |> CCResult.flatten_l
  ;;

  type t =
    { values : Values.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
end

module EmailSuffix = struct
  type t = string [@@deriving eq, show, yojson]

  let value m = m
  let list_value m = CCList.map value m

  let create suffix =
    if CCString.length suffix <= 0
    then Error "Invalid email suffix!"
    else Ok suffix
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "email_suffix")
      CCList.pure
      "email_suffix"
  ;;
end

module TenantEmailSuffixes = struct
  module Values = struct
    type t = EmailSuffix.t list [@@deriving eq, show, yojson]

    let to_string m = m |> to_yojson |> Yojson.Safe.to_string
    let of_string m = m |> Yojson.Safe.from_string |> of_yojson
  end

  type t =
    { values : Values.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }

  type values = t -> EmailSuffix.t list

  let values m = m.values

  let create suffixes =
    if CCList.length suffixes <= 0
    then Error "Provide at least one email suffix."
    else CCList.map EmailSuffix.create suffixes |> CCResult.flatten_l
  ;;
end

module TenantContactEmail = struct
  type t =
    { value : ContactEmail.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Ptime.Span.t [@@deriving show]
  end

  module Warning = struct
    type t = Ptime.Span.t [@@deriving show]
  end
end

module TermsAndConditions : sig
  type t

  val create : string -> t
  val value : t -> string
end = struct
  type t = string

  let create m = m
  let value m = m
end

module SettingValue = struct
  type t =
    | Languages of Language.t list
    | EmailContact of ContactEmail.t
    | EmailSuffixes of EmailSuffix.t list
    | UserSetToInactiveAfter of InactiveUser.DisableAfter.t
    | UserSendWarningBeforeInactive of InactiveUser.Warning.t
    | TermsAndConditions of TermsAndConditions.t

  let value = function
    | Languages languages ->
      String.concat ", " (CCList.map Language.code languages)
    | EmailContact email -> ContactEmail.value email
    | EmailSuffixes suffixes ->
      String.concat ", " (CCList.map EmailSuffix.value suffixes)
    | UserSetToInactiveAfter m -> InactiveUser.DisableAfter.show m
    | UserSendWarningBeforeInactive m -> InactiveUser.Warning.show m
    | TermsAndConditions terms -> TermsAndConditions.value terms
  ;;
end

module Setting = struct
  type t =
    { setting : SettingValue.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
end

type t =
  { setting : SettingValue.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

let value { setting; _ } = SettingValue.value setting
