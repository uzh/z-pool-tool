module System : sig
  module ActiveContacts : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module PendingContactImports : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module LoginCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module SignUpCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module TermsAcceptedCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module AssignmentsCreated : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module InvitationsSent : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module RemindersSent : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module EmailsSent : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
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
    ; terms_last_changed : Pool_common.Model.Ptime.t
    ; assignments_created : AssignmentsCreated.t
    ; invitations_sent : InvitationsSent.t
    ; reminders_sent : RemindersSent.t
    ; emails_sent : EmailsSent.t
    }

  val create : Pool_database.Label.t -> ?period:period -> unit -> t Lwt.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Guard : sig
  module Access : sig
    val read : Guard.ValidationSet.t
  end
end

module Experiment : sig
  module SentInvitations : sig
    type sent_by_count = int * int

    type t =
      { total_sent : int
      ; total_match_filter : int
      ; sent_by_count : sent_by_count list
      }

    val create
      :  Pool_database.Label.t
      -> Experiment.t
      -> (t, Pool_common.Message.error) Lwt_result.t
  end

  module RegistrationPossible : sig
    include Pool_common.Model.BooleanSig

    val field : Pool_common.Message.Field.t
    val hint : Pool_common.I18n.hint
  end

  module SendingInvitations : sig
    type t =
      | No
      | Sending
      | Scheduled

    val show : t -> string
    val field : Pool_common.Message.Field.t
    val hint : Pool_common.I18n.hint
  end

  module SessionCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module ShowUpCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module NotMatchingFilerCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module NoShowCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  module ParticipationCount : sig
    include Pool_common.Model.IntegerSig

    val field : Pool_common.Message.Field.t
  end

  type t =
    { registration_possible : RegistrationPossible.t
    ; sending_invitations : SendingInvitations.t
    ; session_count : SessionCount.t
    ; invitations : SentInvitations.t
    ; not_matching_filter : NotMatchingFilerCount.t
    ; showup_count : ShowUpCount.t
    ; noshow_count : NoShowCount.t
    ; participation_count : ParticipationCount.t
    }

  val registration_possible : t -> RegistrationPossible.t
  val sending_invitations : t -> SendingInvitations.t
  val session_count : t -> SessionCount.t
  val invitations : t -> SentInvitations.t
  val not_matching_filter : t -> NotMatchingFilerCount.t
  val showup_count : t -> ShowUpCount.t
  val noshow_count : t -> NoShowCount.t
  val participation_count : t -> ParticipationCount.t

  val create
    :  Pool_database.Label.t
    -> Experiment.t
    -> (t, Pool_common.Message.error) Lwt_result.t
end
