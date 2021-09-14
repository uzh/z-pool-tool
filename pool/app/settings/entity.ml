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
    | _ -> Error "Invalid Language privided"
  ;;

  let t =
    Caqti_type.(
      custom ~encode:(fun m -> m |> code |> Result.ok) ~decode:of_string string)
  ;;

  let label country_code = country_code |> code |> Utils.Countries.find
end

module EmailSuffix = struct
  type t = string
end

module ContactEmail : sig
  type t
end = struct
  type t = string
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Ptime.Span.t
  end

  module Warning = struct
    type t = Ptime.Span.t
  end
end

module SettingValue = struct
  type t =
    | Languages of Language.t list
    | EmailContact of ContactEmail.t
    | EmailSuffixes of EmailSuffix.t list
    | UserSetToInactiveAfter of InactiveUser.DisableAfter.t
    | UserSendWarningBeforeInactive of InactiveUser.Warning.t
end

module Setting = struct
  type t =
    { setting : SettingValue.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
end
