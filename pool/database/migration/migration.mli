type step =
  { label : string
  ; statement : string
  ; check_fk : bool
  }

val equal_step : step -> step -> Ppx_deriving_runtime.bool

val pp_step
  :  Ppx_deriving_runtime.Format.formatter
  -> step
  -> Ppx_deriving_runtime.unit

val show_step : step -> Ppx_deriving_runtime.string

type steps = step list

val equal_steps : steps -> steps -> Ppx_deriving_runtime.bool

val pp_steps
  :  Ppx_deriving_runtime.Format.formatter
  -> steps
  -> Ppx_deriving_runtime.unit

val show_steps : steps -> Ppx_deriving_runtime.string

type t = string * steps

val equal : t -> t -> Ppx_deriving_runtime.bool
val show : t -> Ppx_deriving_runtime.string
val name : string

exception Exception of string
exception Dirty_migration

val to_sexp : string * steps -> Sexplib0.Sexp.t
val pp : Format.formatter -> string * steps -> unit
val empty : 'a -> 'a * 'b list
val create_step : label:string -> ?check_fk:bool -> string -> step
val add_step : 'a -> 'b * 'a list -> 'b * 'a list
val register_migration : t -> unit
val register_migrations : t list -> unit
val execute : Entity.Label.t -> t list -> unit Lwt.t
val run_all : Entity.Label.t -> unit -> unit Lwt.t

val migrations_status
  :  Entity.Label.t
  -> ?migrations:t list
  -> unit
  -> (string * int option) list Lwt.t

val check_migrations_status
  :  Entity.Label.t
  -> ?migrations:t list
  -> unit
  -> unit Lwt.t

val pending_migrations : Entity.Label.t -> unit -> (string * int) list Lwt.t
val start : Entity.Label.t -> unit -> unit Lwt.t
val extend_migrations : (string * steps) list -> unit -> (string * steps) list

val run_pending_migrations
  :  Entity.Label.t list
  -> (string * steps) list
  -> unit Lwt.t
