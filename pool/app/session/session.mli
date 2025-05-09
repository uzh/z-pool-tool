module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module InternalDescription : sig
  include Pool_model.Base.StringSig
end

module PublicDescription : sig
  include Pool_model.Base.StringSig
end

module ParticipantAmount : sig
  include Pool_model.Base.BaseSig

  val value : t -> int
  val create : int -> (t, Pool_message.Error.t) result
  val schema : Pool_message.Field.t -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Start : sig
  include Pool_model.Base.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

module End : sig
  include Pool_model.Base.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
  val build : Start.t -> Ptime.Span.t -> (t, Pool_message.Error.t) Result.t
end

module Duration : sig
  include Pool_model.Base.DurationSig
end

type base =
  { start : Start.t
  ; duration : int
  ; duration_unit : Pool_model.Base.TimeUnit.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : int option
  ; email_reminder_lead_time_unit : Pool_model.Base.TimeUnit.t option
  ; text_message_reminder_lead_time : int option
  ; text_message_reminder_lead_time_unit : Pool_model.Base.TimeUnit.t option
  }

type reschedule =
  { start : Start.t
  ; duration : Duration.t
  }

module AssignmentCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_message.Error.t) result
end

module NoShowCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_message.Error.t) result
end

module ParticipantCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_message.Error.t) result
end

module CancellationReason : sig
  include Pool_model.Base.StringSig
end

module CanceledAt : sig
  include Pool_model.Base.PtimeSig

  val create : Ptime.t -> (t, Pool_message.Error.t) result
end

type t =
  { id : Id.t
  ; follow_up_to : Id.t option
  ; has_follow_ups : bool
  ; start : Start.t
  ; duration : Duration.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : Pool_common.Reminder.EmailLeadTime.t option
  ; email_reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; text_message_reminder_lead_time : Pool_common.Reminder.TextMessageLeadTime.t option
  ; text_message_reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; assignment_count : AssignmentCount.t
  ; no_show_count : NoShowCount.t
  ; participant_count : ParticipantCount.t
  ; closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; experiment : Experiment.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val create
  :  ?id:Id.t
  -> ?internal_description:InternalDescription.t
  -> ?public_description:PublicDescription.t
  -> ?email_reminder_lead_time:Pool_common.Reminder.EmailLeadTime.t
  -> ?follow_up_to:Id.t
  -> ?has_follow_ups:bool
  -> ?text_message_reminder_lead_time:Pool_common.Reminder.TextMessageLeadTime.t
  -> Start.t
  -> Duration.t
  -> Pool_location.t
  -> ParticipantAmount.t
  -> ParticipantAmount.t
  -> ParticipantAmount.t
  -> Experiment.t
  -> t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val is_canceled_error : Ptime.t -> ('a, Pool_message.Error.t) result
val is_fully_booked : t -> bool
val available_spots : t -> int
val has_assignments : t -> bool
val session_date_to_human : t -> string
val start_end_with_duration_human : t -> string
val start_end_human : t -> string

type event =
  | Created of t
  | Canceled of t
  | Closed of t
  | Deleted of t
  | Updated of (t * t)
  | EmailReminderSent of t
  | TextMsgReminderSent of t
  | Rescheduled of (t * reschedule)

val handle_event : ?user_uuid:Pool_common.Id.t -> Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Public : sig
  type t =
    { id : Id.t
    ; experiment_id : Experiment.Id.t
    ; experiment_title : Experiment.PublicTitle.t
    ; follow_up_to : Id.t option
    ; start : Start.t
    ; duration : Duration.t
    ; description : PublicDescription.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    ; closed_at : Ptime.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val is_fully_booked : t -> bool
  val assignment_creatable : t -> (unit, Pool_message.Error.t) result
  val group_and_sort : t list -> (t * t list) list
  val get_session_end : t -> Ptime.t
  val start_end_with_duration_human : t -> string
  val is_past : t -> bool
  val column_experiment_title : Query.Column.t
  val column_past : Query.Column.t
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list
  val filterable_by : Query.Filter.human option
  val default_query : Query.t
end

val to_public : t -> Public.t

module Calendar : sig
  type location =
    { id : Pool_location.Id.t
    ; name : Pool_location.Name.t
    }

  type links =
    { experiment : string option
    ; session : string option
    }

  type t =
    { id : Id.t
    ; experiment_id : Experiment.Id.t
    ; title : Experiment.Title.t
    ; contact_email : Pool_user.EmailAddress.t option
    ; start : Start.t
    ; end_ : End.t
    ; links : links
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; internal_description : InternalDescription.t option
    ; location : location
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val yojson_of_t : t -> Yojson.Safe.t
  val compare : t -> t -> int
end

val group_and_sort : t list -> (t * t list) list
val is_cancellable : t -> (unit, Pool_message.Error.t) result
val is_closable : t -> (unit, Pool_message.Error.t) result
val is_cancelable : t -> (unit, Pool_message.Error.t) result
val is_deletable : t -> (unit, Pool_message.Error.t) result
val assignments_cancelable : t -> (unit, Pool_message.Error.t) result
val assignments_session_changeable : t -> (unit, Pool_message.Error.t) result
val assignment_creatable : t -> (unit, Pool_message.Error.t) result
val can_be_assigned_to_existing_assignment : t -> (unit, Pool_message.Error.t) result
val reminder_resendable : t -> (unit, Pool_message.Error.t) result
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_multiple : Database.Label.t -> Id.t list -> t list Lwt.t

val find_contact_is_assigned_by_experiment
  :  Database.Label.t
  -> Contact.Id.t
  -> Experiment.Id.t
  -> t list Lwt.t

val find_public
  :  Database.Label.t
  -> Id.t
  -> (Public.t, Pool_message.Error.t) Lwt_result.t

val find_all_for_experiment : Database.Label.t -> Experiment.Id.t -> t list Lwt.t
val find_upcoming_for_experiment : Database.Label.t -> Experiment.Id.t -> t list Lwt.t

val find_all_to_assign_from_waitinglist
  :  Database.Label.t
  -> Experiment.Id.t
  -> t list Lwt.t

val find_all_public_for_experiment
  :  Database.Label.t
  -> Contact.t
  -> Experiment.Id.t
  -> (Public.t list, Pool_message.Error.t) Lwt_result.t

val find_all_ids_of_contact_id : Database.Label.t -> Contact.Id.t -> Id.t list Lwt.t

val find_public_by_assignment
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (Public.t, Pool_message.Error.t) Lwt_result.t

val query_by_contact
  :  ?query:Query.t
  -> Database.Label.t
  -> Contact.t
  -> (Public.t list * Query.t) Lwt.t

val find_by_contact_and_experiment
  :  Database.Label.t
  -> Contact.t
  -> Experiment.Id.t
  -> [< `Canceled | `Past | `Upcoming ]
  -> Public.t list Lwt.t

val has_upcoming_sessions : Database.Label.t -> Contact.Id.t -> bool Lwt.t

val find_by_assignment
  :  Database.Label.t
  -> Id.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val find_experiment_id_and_title
  :  Database.Label.t
  -> Id.t
  -> (Experiment.Id.t * string, Pool_message.Error.t) Lwt_result.t

val find_sessions_to_remind
  :  Pool_tenant.t
  -> (t list * t list, Pool_message.Error.t) Lwt_result.t

val find_follow_ups : Database.Label.t -> Id.t -> t list Lwt.t

val find_open_with_follow_ups
  :  Database.Label.t
  -> Id.t
  -> (t list, Pool_message.Error.t) Lwt_result.t

val find_open : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t

val calendar_by_user
  :  start_time:Ptime.t
  -> end_time:Ptime.t
  -> Database.Label.t
  -> Guard.Persistence.actor
  -> Guard.PermissionOnTarget.t list
  -> Calendar.t list Lwt.t

val calendar_by_location
  :  location_uuid:Pool_location.Id.t
  -> start_time:Ptime.t
  -> end_time:Ptime.t
  -> Database.Label.t
  -> Guard.Persistence.actor
  -> Guard.PermissionOnTarget.t list
  -> Calendar.t list Lwt.t

val query_grouped_by_experiment
  :  ?query:Query.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> ((t * t list) list * Query.t) Lwt.t

val query_by_experiment
  :  ?query:Query.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t) Lwt.t

val find_sessions_to_update_matcher
  :  Database.Label.t
  -> [< `Experiment of Experiment.Id.t | `Upcoming ]
  -> t list Lwt.t

val find_incomplete_by_admin
  :  ?query:Query.t
  -> Guard.Actor.t
  -> Database.Label.t
  -> (t list * Query.t) Lwt.t

val find_upcoming_by_admin
  :  ?query:Query.t
  -> Guard.Actor.t
  -> Database.Label.t
  -> (t list * Query.t) Lwt.t

val to_email_text : Pool_common.Language.t -> t -> string
val follow_up_sessions_to_email_list : t list -> string
val public_to_email_text : Pool_common.Language.t -> Public.t -> string
val find_all_to_swap_by_experiment : Database.Label.t -> Experiment.Id.t -> t list Lwt.t
val column_date : Query.Column.t
val column_no_assignments : Query.Column.t
val column_noshow_count : Query.Column.t
val column_participation_count : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_filter : Query.Filter.t
val default_sort : Query.Sort.t
val default_query : Query.t
val incomplete_default_query : Query.t
val participation_default_query : Query.t

module Repo : sig
  val sql_select_columns : string list
  val joins : string

  module Id : sig
    val t : Id.t Caqti_type.t
  end

  module Start : sig
    val t : Start.t Caqti_type.t
  end

  module End : sig
    val t : End.t Caqti_type.t
  end

  module Duration : sig
    val t : Duration.t Caqti_type.t
  end

  val t : t Caqti_type.t
end

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> Id.t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index_permission : Guard.Permission.t
    val index : ?model:Role.Target.t -> Experiment.Id.t -> Guard.ValidationSet.t
    val create : ?model:Role.Target.t -> Experiment.Id.t -> Guard.ValidationSet.t
    val read : ?model:Role.Target.t -> Experiment.Id.t -> Id.t -> Guard.ValidationSet.t

    val read_by_location
      :  ?model:Role.Target.t
      -> Pool_location.Id.t
      -> Id.t
      -> Guard.ValidationSet.t

    val update : ?model:Role.Target.t -> Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val delete : ?model:Role.Target.t -> Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val close : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val update_permission_on_target : Id.t -> Guard.PermissionOnTarget.t
  end
end

module VersionHistory : Changelog.TSig with type record = t
