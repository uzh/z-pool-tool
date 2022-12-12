type update =
  { start_at : Ptime.t
  ; end_at : Ptime.t
  ; rate : int
  ; distribution : Entity.Distribution.t option
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type event =
  | Created of (Entity.t * Experiment.Id.t)
  | Updated of (update * Entity.t)
  | Deleted of Entity.t
  | Stopped of Entity.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
