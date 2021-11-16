module ContactEmail : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module EmailSuffix : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> t
  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module InactiveUser : sig
  module DisableAfter : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> int
    val to_timespan : t -> Ptime.span
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Warning : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
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
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val value : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Value : sig
  type t
end

type t

val action_of_param
  :  string
  -> ( [> `CreateTenantEmailSuffix
       | `DeleteTenantEmailSuffix
       | `UpdateInactiveUserDisableAfter
       | `UpdateInactiveUserWarning
       | `UpdateTenantContactEmail
       | `UpdateTenantEmailSuffixes
       | `UpdateTenantLanguages
       | `UpdateTermsAndConditions
       ]
     , Pool_common.Message.error )
     result

val stringify_action
  :  [< `CreateTenantEmailSuffix
     | `DeleteTenantEmailSuffix
     | `UpdateInactiveUserDisableAfter
     | `UpdateInactiveUserWarning
     | `UpdateTenantContactEmail
     | `UpdateTenantEmailSuffixes
     | `UpdateTenantLanguages
     | `UpdateTermsAndConditions
     ]
  -> string

type event =
  | LanguagesUpdated of Pool_common.Language.t list
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
  -> Pool_common.Language.t list Lwt.t

val find_email_suffixes
  :  Pool_common.Database.Label.t
  -> EmailSuffix.t list Lwt.t

val find_contact_email : Pool_common.Database.Label.t -> ContactEmail.t Lwt.t

val find_inactive_user_disable_after
  :  Pool_common.Database.Label.t
  -> InactiveUser.DisableAfter.t Lwt.t

val find_inactive_user_warning
  :  Pool_common.Database.Label.t
  -> InactiveUser.Warning.t Lwt.t

val find_terms_and_conditions
  :  Pool_common.Database.Label.t
  -> TermsAndConditions.t Lwt.t

val terms_and_conditions_last_updated
  :  Pool_common.Database.Label.t
  -> Ptime.t Lwt.t
