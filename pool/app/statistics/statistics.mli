module ActiveContacts : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module PendingContactImports : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module LoginCount : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module SignUpCount : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module TermsAcceptedCount : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module AssignmentsCreated : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module InvitationsSent : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module RemindersSent : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

module EmailsSent : sig
  include Pool_model.Base.IntegerSig

  val field : Pool_message.Field.t
end

type period =
  | Min15
  | Hour1
  | Day1
  | Month1

val show_period : period -> string
val default_period : period
val all_periods : period list
val read_period : string -> period option
val period_to_human : Pool_common.Language.t -> period -> string

type t =
  { active_contacts : ActiveContacts.t
  ; pending_contact_imports : PendingContactImports.t
  ; login_count : LoginCount.t
  ; sign_up_count : SignUpCount.t
  ; terms_accepted_count : TermsAcceptedCount.t
  ; terms_last_changed : Pool_model.Base.Ptime.t
  ; assignments_created : AssignmentsCreated.t
  ; invitations_sent : InvitationsSent.t
  ; reminders_sent : RemindersSent.t
  ; emails_sent : EmailsSent.t
  }

val create : Database.Label.t -> ?period:period -> unit -> t Lwt.t
val yojson_of_t : t -> Yojson.Safe.t

module ExperimentFilter : sig
  type t =
    { invitations : Experiment.Statistics.SentInvitations.statistics
    ; assigned_contacts_not_matching : int
    }

  val create
    :  Database.Label.t
    -> Experiment.t
    -> Filter.query option
    -> (t, Pool_message.Error.t) Lwt_result.t
end

module Guard : sig
  module Access : sig
    val read : Guard.ValidationSet.t
  end
end
