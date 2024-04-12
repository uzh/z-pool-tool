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
  ; closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; experiment : Experiment.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
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
  -> (Session.Duration.t, Pool_common.Message.error) result

val has_assignments : t -> bool
val is_deletable : t -> bool
val is_closable : t -> (unit, Pool_common.Message.error) result
val start_end_with_duration_human : t -> string

type create =
  { start : Session.Start.t
  ; end_at : Session.End.t
  ; internal_description : Session.InternalDescription.t option
  ; public_description : Session.PublicDescription.t option
  ; max_participants : Session.ParticipantAmount.t option
  }

type event = Created of t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

val find_overlapping
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> start:Session.Start.t
  -> end_at:Session.End.t
  -> t list Lwt.t

val query_by_experiment
  :  ?query:Query.t
  -> Pool_database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t) Lwt.t
