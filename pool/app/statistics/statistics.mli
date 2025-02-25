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

module Pool : sig
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
  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module ExperimentInvitations : sig
  type t =
    { invitation_resets : Experiment.InvitationReset.t list
    ; sent_since_last_reset : int
    ; total_match_filter : int
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val create
    :  ?total_match_filter:int
    -> Database.Label.t
    -> Experiment.t
    -> (t, Pool_message.Error.t) Lwt_result.t
end

module ExperimentFilter : sig
  type t =
    { invited_contacts_count : int
    ; total_match_filter : int
    ; total_uninvited_matching : int
    ; assigned_contacts_not_matching : int
    ; sent_invitations : ExperimentInvitations.t
    }

  val create
    :  Database.Label.t
    -> Experiment.t
    -> Filter.query option
    -> (t, Pool_message.Error.t) Lwt_result.t
end

module ExperimentOverview : sig
  module RegistrationPossible : sig
    include Pool_model.Base.BooleanSig

    val field : Pool_message.Field.t
    val hint : Pool_common.I18n.hint
  end

  module SessionCount : sig
    include Pool_model.Base.IntegerSig

    val field : Pool_message.Field.t
  end

  module ShowUpCount : sig
    include Pool_model.Base.IntegerSig

    val field : Pool_message.Field.t
  end

  module NoShowCount : sig
    include Pool_model.Base.IntegerSig

    val field : Pool_message.Field.t
  end

  module ParticipationCount : sig
    include Pool_model.Base.IntegerSig

    val field : Pool_message.Field.t
  end

  type t =
    { registration_possible : RegistrationPossible.t
    ; sending_invitations : Experiment.SendingInvitations.t
    ; session_count : SessionCount.t
    ; invitations : ExperimentInvitations.t
    ; showup_count : ShowUpCount.t
    ; noshow_count : NoShowCount.t
    ; participation_count : ParticipationCount.t
    }

  val create : Database.Label.t -> Experiment.t -> (t, Pool_message.Error.t) result Lwt.t
end

module Guard : sig
  module Access : sig
    val read : Guard.ValidationSet.t
  end
end
