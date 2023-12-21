module ContactEmail : sig
  include Pool_common.Model.StringSig
end

module EmailSuffix : sig
  include Pool_common.Model.StringSig
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

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  module Warning : sig
    type t

    val create : string -> (t, Pool_common.Message.error) result
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> int
    val to_timespan : t -> Ptime.span

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end
end

module TriggerProfileUpdateAfter : sig
  type t

  val create : string -> (t, Pool_common.Message.error) result
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> int
  val to_timespan : t -> Ptime.span

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module TermsAndConditions : sig
  module Terms : sig
    include Pool_common.Model.StringSig
  end

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : string -> string -> (t, Pool_common.Message.error) result
  val value : t -> Pool_common.Language.t * Terms.t
end

module Value : sig
  type t
end

type t

val action_of_param
  :  string
  -> ( [> `CreateEmailSuffix
       | `DeleteEmailSuffix
       | `UpdateDefaultLeadTime
       | `UpdateTextMsgDefaultLeadTime
       | `UpdateInactiveUserDisableAfter
       | `UpdateInactiveUserWarning
       | `UpdateContactEmail
       | `UpdateEmailSuffixes
       | `UpdateLanguages
       | `UpdateTriggerProfileUpdateAfter
       ]
       , Pool_common.Message.error )
       result

val stringify_action
  :  [< `CreateEmailSuffix
     | `DeleteEmailSuffix
     | `UpdateDefaultLeadTime
     | `UpdateTextMsgDefaultLeadTime
     | `UpdateInactiveUserDisableAfter
     | `UpdateInactiveUserWarning
     | `UpdateContactEmail
     | `UpdateEmailSuffixes
     | `UpdateLanguages
     | `UpdateTriggerProfileUpdateAfter
     ]
  -> string

type event =
  | ContactEmailUpdated of ContactEmail.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.LeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of Pool_common.Reminder.LeadTime.t
  | EmailSuffixesUpdated of EmailSuffix.t list
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val find_languages : Pool_database.Label.t -> Pool_common.Language.t list Lwt.t
val find_email_suffixes : Pool_database.Label.t -> EmailSuffix.t list Lwt.t
val find_contact_email : Pool_database.Label.t -> ContactEmail.t Lwt.t

val find_inactive_user_disable_after
  :  Pool_database.Label.t
  -> InactiveUser.DisableAfter.t Lwt.t

val find_inactive_user_warning
  :  Pool_database.Label.t
  -> InactiveUser.Warning.t Lwt.t

val find_trigger_profile_update_after
  :  Pool_database.Label.t
  -> TriggerProfileUpdateAfter.t Lwt.t

val default_language_of_list
  :  Pool_common.Language.t list
  -> Pool_common.Language.t

val default_language : Pool_database.Label.t -> Pool_common.Language.t Lwt.t

val find_default_reminder_lead_time
  :  Pool_database.Label.t
  -> Pool_common.Reminder.LeadTime.t Lwt.t

val find_default_text_msg_reminder_lead_time
  :  Pool_database.Label.t
  -> Pool_common.Reminder.LeadTime.t Lwt.t

val default_email_session_reminder_lead_time_key_yojson : Yojson.Safe.t
val default_text_message_session_reminder_lead_time_key_yojson : Yojson.Safe.t
val trigger_profile_update_after_key_yojson : Yojson.Safe.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> Pool_common.Id.t
      -> (Guard.Target.t, Pool_common.Message.error) result Lwt.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Guard.ValidationSet.t
    val update : Guard.ValidationSet.t
    val delete : Guard.ValidationSet.t
  end
end
