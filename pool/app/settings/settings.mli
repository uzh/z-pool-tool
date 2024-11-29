type command =
  { time_value : int
  ; time_unit : Pool_model.Base.TimeUnit.t
  }

val update_duration_schema
  :  (Pool_message.Error.t, int) Pool_conformist.Field.t
  -> Pool_message.Field.t
  -> ( Pool_message.Error.t
       , int -> Pool_model.Base.TimeUnit.t -> command
       , command )
       Pool_conformist.t

module ContactEmail : sig
  include Pool_model.Base.StringSig
end

module EmailSuffix : sig
  include Pool_model.Base.StringSig
end

module InactiveUser : sig
  module DisableAfter : sig
    include Pool_model.Base.DurationSig
  end

  module Warning : sig
    include Pool_model.Base.DurationSig
  end
end

module TriggerProfileUpdateAfter : sig
  include Pool_model.Base.DurationSig
end

module TermsAndConditions : sig
  module Terms : sig
    include Pool_model.Base.StringSig
  end

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : string -> string -> (t, Pool_message.Error.t) result
  val value : t -> Pool_common.Language.t * Terms.t
end

module UserImportReminder : sig
  module FirstReminderAfter : sig
    include Pool_model.Base.DurationSig

    val validate : t -> (t, Pool_message.Error.t) result
  end

  module SecondReminderAfter : sig
    include Pool_model.Base.DurationSig

    val validate : t -> (t, Pool_message.Error.t) result
  end
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
       | `UserImportFirstReminderAfter
       | `UserImportSecondReminderAfter
       ]
       , Pool_message.Error.t )
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
     | `UserImportFirstReminderAfter
     | `UserImportSecondReminderAfter
     ]
  -> string

type event =
  | ContactEmailUpdated of ContactEmail.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.EmailLeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of
      Pool_common.Reminder.TextMessageLeadTime.t
  | EmailSuffixesUpdated of EmailSuffix.t list
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t
  | UserImportFirstReminderAfterUpdated of
      UserImportReminder.FirstReminderAfter.t
  | UserImportSecondReminderAfterUpdated of
      UserImportReminder.SecondReminderAfter.t

val handle_event
  :  ?user_uuid:Pool_common.Id.t
  -> Database.Label.t
  -> event
  -> unit Lwt.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val find_languages : Database.Label.t -> Pool_common.Language.t list Lwt.t
val find_email_suffixes : Database.Label.t -> EmailSuffix.t list Lwt.t
val find_contact_email : Database.Label.t -> ContactEmail.t Lwt.t

val find_inactive_user_disable_after
  :  Database.Label.t
  -> InactiveUser.DisableAfter.t Lwt.t

val find_inactive_user_warning
  :  Database.Label.t
  -> InactiveUser.Warning.t Lwt.t

val find_trigger_profile_update_after
  :  Database.Label.t
  -> TriggerProfileUpdateAfter.t Lwt.t

val default_language_of_list
  :  Pool_common.Language.t list
  -> Pool_common.Language.t

val default_language : Database.Label.t -> Pool_common.Language.t Lwt.t

val find_default_reminder_lead_time
  :  Database.Label.t
  -> Pool_common.Reminder.EmailLeadTime.t Lwt.t

val find_default_text_msg_reminder_lead_time
  :  Database.Label.t
  -> Pool_common.Reminder.TextMessageLeadTime.t Lwt.t

val find_user_import_first_reminder_after
  :  Database.Label.t
  -> UserImportReminder.FirstReminderAfter.t Lwt.t

val find_user_import_second_reminder_after
  :  Database.Label.t
  -> UserImportReminder.SecondReminderAfter.t Lwt.t

val default_email_session_reminder_lead_time_key_yojson : Yojson.Safe.t
val default_text_message_session_reminder_lead_time_key_yojson : Yojson.Safe.t
val trigger_profile_update_after_key_yojson : Yojson.Safe.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> Pool_common.Id.t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

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
