module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module Description : sig
  include Pool_common.Model.StringSig
end

module Limitations : sig
  include Pool_common.Model.StringSig
end

module ParticipantAmount : sig
  include Pool_common.Model.BaseSig

  val value : t -> int
  val create : int -> (t, Pool_common.Message.error) result

  val schema
    :  Pool_common.Message.Field.t
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Start : sig
  include Pool_common.Model.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

module End : sig
  type t
end

module Duration : sig
  include Pool_common.Model.BaseSig

  val value : t -> Ptime.Span.t
  val create : Ptime.Span.t -> (t, Pool_common.Message.error) result
end

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; limitations : Limitations.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }

type update =
  { start : Start.t option
  ; duration : Duration.t option
  ; description : Description.t option
  ; limitations : Limitations.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
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
  val create : int -> (t, Pool_common.Message.error) result
end

module NoShowCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_common.Message.error) result
end

module ParticipantCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_common.Message.error) result
end

module CancellationReason : sig
  include Pool_common.Model.StringSig
end

module CanceledAt : sig
  include Pool_common.Model.PtimeSig
end

type t =
  { id : Id.t
  ; follow_up_to : Id.t option
  ; has_follow_ups : bool
  ; start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; limitations : Limitations.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; assignment_count : AssignmentCount.t
  ; no_show_count : NoShowCount.t
  ; participant_count : ParticipantCount.t
  ; (* TODO [aerben] make type for canceled_at? *)
    closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val create
  :  ?id:Id.t
  -> ?follow_up_to:Id.t
  -> ?has_follow_ups:bool
  -> Start.t
  -> Duration.t
  -> Description.t option
  -> Limitations.t option
  -> Pool_location.t
  -> ParticipantAmount.t
  -> ParticipantAmount.t
  -> ParticipantAmount.t
  -> Pool_common.Reminder.LeadTime.t option
  -> t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val is_fully_booked : t -> bool
val available_spots : t -> int
val has_assignments : t -> bool
val session_date_to_human : t -> string

type event =
  | Created of (t * Experiment.Id.t)
  | Canceled of t
  | Closed of t
  | Deleted of t
  | Updated of (base * Pool_location.t * t)
  | ReminderSent of t
  | Rescheduled of (t * reschedule)

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Public : sig
  type t =
    { id : Id.t
    ; follow_up_to : Id.t option
    ; start : Start.t
    ; duration : Duration.t
    ; description : Description.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val is_fully_booked : t -> bool
  val assignment_creatable : t -> (unit, Pool_common.Message.error) result
  val group_and_sort : t list -> (t * t list) list
  val get_session_end : t -> Ptime.t
end

val to_public : t -> Public.t

module Calendar : sig
  type contact_person =
    { name : string
    ; email : Pool_user.EmailAddress.t
    }

  type location =
    { id : Pool_location.Id.t
    ; name : Pool_location.Name.t
    }

  type t =
    { id : Id.t
    ; title : Experiment.Title.t
    ; start : Start.t
    ; end_ : End.t
    ; session_url : string
    ; experiment_url : string
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; description : Description.t option
    ; location : location
    ; contact_person : contact_person option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val yojson_of_t : t -> Yojson.Safe.t
end

val group_and_sort : t list -> (t * t list) list
val is_cancellable : t -> (unit, Pool_common.Message.error) result
val is_closable : t -> (unit, Pool_common.Message.error) result
val is_deletable : t -> t list -> (unit, Pool_common.Message.error) result
val assignments_cancelable : t -> (unit, Pool_common.Message.error) result
val assignment_creatable : t -> (unit, Pool_common.Message.error) result

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_public
  :  Pool_database.Label.t
  -> Id.t
  -> (Public.t, Pool_common.Message.error) Lwt_result.t

val find_all_public_by_location
  :  Pool_database.Label.t
  -> Pool_location.Id.t
  -> (Public.t list, Pool_common.Message.error) Lwt_result.t

val find_all_for_experiment
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> (t list, Pool_common.Message.error) Lwt_result.t

val find_all_to_assign_from_waitinglist
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> (t list, Pool_common.Message.error) Lwt_result.t

val find_all_public_for_experiment
  :  Pool_database.Label.t
  -> Contact.t
  -> Experiment.Id.t
  -> (Public.t list, Pool_common.Message.error) Lwt_result.t

val find_public_by_assignment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Public.t, Pool_common.Message.error) Lwt_result.t

val find_upcoming_public_by_contact
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> ( (Experiment.Public.t * Public.t * Public.t list) list
     , Pool_common.Message.error )
     result
     Lwt.t

val find_by_assignment
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_experiment_id_and_title
  :  Pool_database.Label.t
  -> Id.t
  -> (Experiment.Id.t * string, Pool_common.Message.error) Lwt_result.t

val find_sessions_to_remind
  :  Pool_database.Label.t
  -> (t list, Pool_common.Message.error) Lwt_result.t

val find_follow_ups
  :  Pool_database.Label.t
  -> Id.t
  -> (t list, Pool_common.Message.error) Lwt_result.t

val find_open_with_follow_ups
  :  Pool_database.Label.t
  -> Id.t
  -> (t list, Pool_common.Message.error) Lwt_result.t

val find_for_calendar_by_location
  :  Pool_location.Id.t
  -> Pool_database.Label.t
  -> start_time:Ptime.t
  -> end_time:Ptime.t
  -> Calendar.t list Lwt.t

val find_for_calendar_by_user
  :  'a Guard.Persistence.actor
  -> Pool_database.Label.t
  -> start_time:Ptime.t
  -> end_time:Ptime.t
  -> Calendar.t list Lwt.t

val to_email_text : Pool_common.Language.t -> t -> string
val follow_up_sessions_to_email_list : t list -> string
val public_to_email_text : Pool_common.Language.t -> Public.t -> string

val has_bookable_spots_for_experiments
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> (bool, Pool_common.Message.error) result Lwt.t

module Repo : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end
end

module Guard : sig
  val relation : ?ctx:(string * string) list -> unit -> unit Lwt.t

  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Target.t Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index_action : Guard.Action.t
    val index : Experiment.Id.t -> Guard.ValidationSet.t
    val create : Experiment.Id.t -> Guard.ValidationSet.t
    val read : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val update : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val delete : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
  end
end
