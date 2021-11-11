module Day = struct
  type t = int
end

module Week = struct
  type t = int
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
    | Languages of Pool_common.Language.t list
    | EmailContact of ContactEmail.t
    | EmailSuffixes of EmailSuffix.t list
    | UserSetToInactiveAfter of InactiveUser.DisableAfter.t
    | UserSendWarningBeforeInactive of InactiveUser.Warning.t
    | TermsAndConditions of TermsAndConditions.t

  let value = function
    | Languages languages ->
      String.concat ", " (CCList.map Pool_common.Language.code languages)
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
