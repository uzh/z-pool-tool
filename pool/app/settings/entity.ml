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
end

module EmailSuffix : sig
  type t
end = struct
  type t = string
end

module ContactEmail : sig
  type t
end = struct
  type t = string
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Week.t
  end

  module WarnUser = struct
    type t = Day.t
  end
end

module SettingValue = struct
  type t =
    | Languages of Language.t list
    | EmailContact of ContactEmail.t
    | EmailSuffixes of EmailSuffix.t list
    | UserSetToInactiveAfter of InactiveUser.DisableAfter.t
    | UserSendWarningBeforeInactive of InactiveUser.WarnUser.t
end

module Setting = struct
  type t =
    { setting : SettingValue.t
    ; created_at : Sihl.timestamp
    ; updated_at : Sihl.timestamp
    }
end
