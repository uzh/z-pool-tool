module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module NoShow : sig
  include Pool_common.Model.BooleanSig

  val field : Pool_common.Message.Field.t
  val init : t
end

module Participated : sig
  include Pool_common.Model.BooleanSig

  val field : Pool_common.Message.Field.t
  val init : t
end

module MatchesFilter : sig
  type t

  val equal : t -> t -> bool
  val value : t -> bool
  val create : bool -> t
  val init : t
end

module CanceledAt : sig
  include Pool_common.Model.PtimeSig
end

module MarkedAsDeleted : sig
  include Pool_common.Model.BooleanSig

  val init : t
end

module ExternalDataId : sig
  include Pool_common.Model.StringSig

  val field : Pool_common.Message.Field.t
end

type t =
  { id : Id.t
  ; contact : Contact.t
  ; no_show : NoShow.t option
  ; participated : Participated.t option
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t option
  ; marked_as_deleted : MarkedAsDeleted.t
  ; external_data_id : ExternalDataId.t option
  ; reminder_manually_last_sent_at : Pool_common.Reminder.SentAt.t option
  ; custom_fields : Custom_field.Public.t list option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val show : t -> string

val create
  :  ?id:Id.t
  -> ?no_show:NoShow.t
  -> ?participated:Participated.t
  -> ?matches_filter:MatchesFilter.t
  -> ?canceled_at:CanceledAt.t
  -> ?marked_as_deleted:MarkedAsDeleted.t
  -> ?external_data_id:ExternalDataId.t
  -> ?reminder_manually_last_sent_at:Pool_common.Reminder.SentAt.t
  -> Contact.t
  -> t

module ExternalDataIdentifier : sig
  type t =
    { external_data_id : ExternalDataId.t
    ; experiment_id : Experiment.Id.t
    ; experiment_title : Experiment.Title.t
    ; session_id : Session.Id.t
    ; session_start : Session.Start.t
    ; session_duration : Session.Duration.t
    }
end

val is_deletable : t -> (unit, Pool_common.Message.error) result
val is_cancellable : t -> (unit, Pool_common.Message.error) result
val attendance_settable : t -> (unit, Pool_common.Message.error) result

val session_changeable
  :  Session.t
  -> t
  -> (unit, Pool_common.Message.error) result

val reminder_sendable
  :  Session.t
  -> t
  -> (unit, Pool_common.Message.error) result

module Public : sig
  type t =
    { id : Id.t
    ; canceled_at : CanceledAt.t option
    }
end

module IncrementParticipationCount : sig
  type t

  val value : t -> bool
  val create : bool -> t
end

val validate
  :  Experiment.t
  -> t
  -> (unit, Pool_common.Message.error list) result

val set_close_default_values : t -> t * NoShow.t * Participated.t
val boolean_fields : Pool_common.Message.Field.t list

type session_counters =
  { total : int
  ; num_no_shows : int
  ; num_participations : int
  }

val counters_of_session
  :  Pool_database.Label.t
  -> Session.Id.t
  -> session_counters Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_closed
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_upcoming_public_by_experiment_and_contact_opt
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> Public.t list Lwt.t

val find_past_public_by_experiment_and_contact_opt
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> Public.t list Lwt.t

val find_all_public_by_experiment_and_contact_opt
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> Public.t list Lwt.t

val assignment_to_experiment_exists
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> bool Lwt.t

val find_by_contact_and_experiment
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> (Session.t * t) list Lwt.t

val find_not_deleted_by_session
  :  Pool_database.Label.t
  -> Session.Id.t
  -> t list Lwt.t

val find_all_by_session : Pool_database.Label.t -> Session.Id.t -> t list Lwt.t

val find_multiple_by_session
  :  Pool_database.Label.t
  -> Session.Id.t
  -> Id.t list
  -> t list Lwt.t

val query_by_session
  :  ?query:Query.t
  -> Pool_database.Label.t
  -> Session.Id.t
  -> (t list * Query.t) Lwt.t

val find_uncanceled_by_session
  :  Pool_database.Label.t
  -> Session.Id.t
  -> t list Lwt.t

val find_for_session_close_screen
  :  Pool_database.Label.t
  -> Session.Id.t
  -> (t list * Custom_field.t list) Lwt.t

val find_for_session_detail_screen
  :  query:Query.t
  -> Pool_database.Label.t
  -> Session.Id.t
  -> ((t list * Custom_field.t list) * Query.t) Lwt.t

val find_deleted_by_session
  :  Pool_database.Label.t
  -> Session.Id.t
  -> t list Lwt.t

val count_unsuitable_by
  :  Pool_database.Label.t
  -> [ `Experiment of Experiment.Id.t | `Session of Session.Id.t ]
  -> int Lwt.t

val find_with_follow_ups : Pool_database.Label.t -> Id.t -> t list Lwt.t
val find_follow_ups : Pool_database.Label.t -> t -> t list Lwt.t

val find_upcoming_by_experiment
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> ( Experiment.t * (Session.t * t list) list
       , Pool_common.Message.error )
       Lwt_result.t

val find_upcoming
  :  Pool_database.Label.t
  -> (Experiment.t * (Session.t * t list) list) list Lwt.t

val contact_participation_in_other_assignments
  :  Pool_database.Label.t
  -> exclude_assignments:t list
  -> Experiment.Id.t
  -> Contact.Id.t
  -> (bool, Pool_common.Message.error) Lwt_result.t

val find_external_data_identifiers_by_contact
  :  Pool_database.Label.t
  -> Contact.Id.t
  -> ExternalDataIdentifier.t list Lwt.t

val group_by_contact : t list -> (Contact.t * t list) list
val column_canceled_at : Query.Column.t
val column_no_show : Query.Column.t
val column_participated : Query.Column.t
val column_external_data_id : Query.Column.t
val column_external_data_id_abbr : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

type event =
  | Canceled of t
  | Created of (t * Session.Id.t)
  | MarkedAsDeleted of t
  | ExternalDataIdUpdated of t * ExternalDataId.t option
  | Updated of t

val canceled : t -> event
val created : t * Session.Id.t -> event
val markedasdeleted : t -> event
val updated : t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val create : Experiment.Id.t -> Guard.ValidationSet.t
    val read : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val update : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val delete : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
  end
end
