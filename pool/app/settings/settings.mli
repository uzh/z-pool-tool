module Language : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val code : t -> string
  val of_string : string -> (t, string) result
  val t : t Caqti_type.t
  val label : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
  val all : unit -> t list
  val all_codes : unit -> string list
end

module ContactEmail : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> t
  val value : t -> string
  val create : t -> (t, t) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module EmailSuffix : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> t
  val value : t -> string
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module InactiveUser : sig
  module DisableAfter : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> int
    val to_timespan : t -> Ptime.span
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Warning : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> int
    val to_timespan : t -> Ptime.span
    val schema : unit -> ('a, t) Conformist.Field.t
  end
end

module TermsAndConditions : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> t
  val create : string -> (t, string) result
  val value : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Value : sig
  type t
end

type t

type event =
  | LanguagesUpdated of Language.t list
  | EmailSuffixesUpdated of EmailSuffix.t list
  | ContactEmailUpdated of ContactEmail.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | TermsAndConditionsUpdated of TermsAndConditions.t

val handle_event : Pool_common.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

val find_languages
  :  Pool_common.Database.Label.t
  -> unit
  -> (Language.t list, string) Result.result Lwt.t

val find_email_suffixes
  :  Pool_common.Database.Label.t
  -> unit
  -> (EmailSuffix.t list, string) Result.result Lwt.t

val find_contact_email
  :  Pool_common.Database.Label.t
  -> unit
  -> (ContactEmail.t, string) Result.result Lwt.t

val find_inactive_user_disable_after
  :  Pool_common.Database.Label.t
  -> unit
  -> (InactiveUser.DisableAfter.t, string) Result.result Lwt.t

val find_inactive_user_warning
  :  Pool_common.Database.Label.t
  -> unit
  -> (InactiveUser.Warning.t, string) Result.result Lwt.t

val find_terms_and_conditions
  :  Pool_common.Database.Label.t
  -> unit
  -> (TermsAndConditions.t, string) Result.result Lwt.t

val terms_and_conditions_last_updated
  :  Pool_common.Database.Label.t
  -> (Ptime.t, string) Lwt_result.t
