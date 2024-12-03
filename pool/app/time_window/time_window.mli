type t =
  { id : Session.Id.t
  ; start : Session.Start.t
  ; duration : Session.Duration.t
  ; internal_description : Session.InternalDescription.t option
  ; public_description : Session.PublicDescription.t option
  ; max_participants : Session.ParticipantAmount.t option
  ; assignment_count : Session.AssignmentCount.t
  ; no_show_count : Session.NoShowCount.t
  ; participant_count : Session.ParticipantCount.t
  ; experiment : Experiment.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Session.Id.t
  -> ?internal_description:Session.InternalDescription.t
  -> ?public_description:Session.PublicDescription.t
  -> ?max_participants:Session.ParticipantAmount.t
  -> Session.Start.t
  -> Session.Duration.t
  -> Experiment.t
  -> t

val ends_at : t -> Ptime.t

val duration
  :  start:Session.Start.t
  -> end_at:Session.End.t
  -> (Session.Duration.t, Pool_message.Error.t) result

val has_assignments : t -> bool
val is_deletable : t -> bool
val is_closed : t -> bool
val start_end_with_duration_human : t -> string

type create =
  { start : Session.Start.t
  ; end_at : Session.End.t
  ; internal_description : Session.InternalDescription.t option
  ; public_description : Session.PublicDescription.t option
  ; max_participants : Session.ParticipantAmount.t option
  }

type event =
  | Created of t
  | Updated of t
  | Deleted of t

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val find : Database.Label.t -> Session.Id.t -> (t, Pool_message.Error.t) Lwt_result.t

val find_overlapping
  :  ?exclude:Session.Id.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> start:Session.Start.t
  -> end_at:Session.End.t
  -> t list Lwt.t

val query_by_experiment
  :  ?query:Query.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t) Lwt.t

val find_current_by_experiment : Database.Label.t -> Experiment.Id.t -> t option Lwt.t
val find_upcoming_by_experiment : Database.Label.t -> Experiment.Id.t -> t option Lwt.t
