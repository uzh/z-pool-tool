module Day = struct
  type t = int
end

module Week = struct
  type t = int
end

module Language = struct
  type t =
    | En
    | De
  [@@deriving eq, show]

  let code = function
    | En -> "EN"
    | De -> "DE"
  ;;

  let of_string = function
    | "EN" -> Ok En
    | "DE" -> Ok De
    | _ -> Error Pool_common.Error.(Invalid Language)
  ;;

  let t =
    let open CCResult in
    (* TODO: Belongs to Repo (search for all caqti types in entities) *)
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> code |> pure)
        ~decode:(fun m -> map_err (fun _ -> "decode language") @@ of_string m)
        string)
  ;;

  let label country_code = country_code |> code |> Utils.Countries.find

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder of_string Error.Language)
      (fun l -> [ code l ])
      "default_language"
  ;;
end

module EmailSuffix = struct
  type t = string

  let value m = m
end

module ContactEmail : sig
  type t

  val value : t -> string
end = struct
  type t = string

  let value m = m
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

type t =
  { setting : SettingValue.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

let value { setting; _ } = SettingValue.value setting
