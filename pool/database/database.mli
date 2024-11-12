module Guard : Guardian_backend.Pools.Sig

module Caqti_encoders : sig
  module Data : sig
    type _ t =
      | [] : unit t
      | ( :: ) : ('a * 'b t) -> ('a * 'b) t

    val make_value : 'a t -> 'a
  end

  module Schema : sig
    type _ t =
      | [] : unit t
      | ( :: ) : ('a Caqti_type.t * 'b t) -> ('a * 'b) t

    val make_type : 'a t -> 'a Caqti_type.t
  end

  val custom
    :  encode:('b -> ('a Data.t, string) result)
    -> decode:('a -> ('b, string) result)
    -> 'a Schema.t
    -> 'b Caqti_type.t

  val custom_ok
    :  encode:('b -> 'a Data.t)
    -> decode:('a -> 'b)
    -> 'a Schema.t
    -> 'b Caqti_type.t
end

module Dynparam : sig
  type t = Pack : 'a Caqti_type.t * 'a -> t

  val empty : t
  val prefix : 'a Caqti_type.t -> 'a -> t -> t
  val add : 'a Caqti_type.t -> 'a -> t -> t
end

module Label : sig
  include Pool_model.Base.StringSig
end

module Url : sig
  include Pool_model.Base.StringSig
end

module Status : sig
  type t =
    | Active
    | ConnectionIssue
    | Disabled
    | Maintenance
    | MigrationsConnectionIssue
    | MigrationsFailed
    | MigrationsPending

  val create : string -> (t, Pool_message.Error.t) result
  val all : t list
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val read : string -> t
  val of_string : string -> (t, Pool_message.Error.t) result
end

type t

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val create : ?status:Status.t -> Label.t -> Url.t -> t
val label : t -> Label.t
val status : t -> Status.t
val to_ctx : Label.t -> (string * string) list
val of_ctx_opt : (string * string) list -> Label.t option
val of_ctx_exn : (string * string) list -> Label.t

module Repo : sig
  val make_caqti_type
    :  'a Caqti_type.t
    -> ('a -> ('b, Pool_message.Error.t) result)
    -> ('b -> 'a)
    -> 'b Caqti_type.t

  val sql_select_label : string
  val sql_select_columns : string list

  val sql_database_join_on_label
    :  ?join_prefix:string
    -> ?status:[ `All | `List of Status.t list ]
    -> string
    -> string

  val insert_request : (t, unit, [ `Zero ]) Caqti_request.t
  val update_request : (Label.t * t, unit, [ `Zero ]) Caqti_request.t

  module Status : Pool_model.Base.CaqtiSig with type t = Status.t
  module Label : Pool_model.Base.CaqtiSig with type t = Label.t

  val t : t Caqti_type.t
end

module Logger : sig
  module Tags : sig
    val add_label : string Logs.Tag.def
    val add : Label.t -> Logs.Tag.set -> Logs.Tag.set
    val create : Label.t -> Logs.Tag.set
    val extend : Label.t -> Logs.Tag.set option -> Logs.Tag.set
  end
end

module Config : sig
  val database : t
  val database_pool_size : int
  val expected_databases : int
end

val query
  :  Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val collect
  :  Label.t
  -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
  -> 'a
  -> 'b list Lwt.t

val exec : Label.t -> ('a, unit, [< `Zero ]) Caqti_request.t -> 'a -> unit Lwt.t
val find : Label.t -> ('a, 'b, [< `One ]) Caqti_request.t -> 'a -> 'b Lwt.t

val find_opt
  :  Label.t
  -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
  -> 'a
  -> 'b option Lwt.t

val populate
  :  Label.t
  -> string
  -> string list
  -> 'a Caqti_type.t
  -> 'a list
  -> unit Lwt.t

val transaction
  :  ?setup:(Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
  -> ?cleanup:(Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
  -> Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val transaction_iter
  :  Label.t
  -> (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
  -> unit Lwt.t

val exec_query
  :  ('a, unit, [< `Zero ]) Caqti_request.t
  -> 'a
  -> (module Caqti_lwt.CONNECTION)
  -> (unit, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

val exclude_ids
  :  string
  -> ('a -> string)
  -> Dynparam.t
  -> 'a list
  -> (Dynparam.t, string option) CCPair.t

val clean_all : Label.t -> unit Lwt.t

module Migration : sig
  exception Exception of string
  exception Dirty_migration

  module Step : sig
    type t =
      { label : string
      ; statement : string
      ; check_fk : bool
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : label:string -> ?check_fk:bool -> string -> t
  end

  type steps = Step.t list

  val equal_steps : steps -> steps -> bool
  val pp_steps : Format.formatter -> steps -> unit
  val show_steps : steps -> string

  type t = string * steps

  val equal : t -> t -> bool
  val show : t -> string
  val to_sexp : string * steps -> Sexplib0.Sexp.t
  val pp : Format.formatter -> string * steps -> unit
  val empty : 'a -> 'a * 'b list
  val add_step : 'a -> 'b * 'a list -> 'b * 'a list

  (** [register_migration migration] registers a migration [migration] with the
      migration service so it can be executed with `run_all`. *)
  val register_migration : t -> unit

  (** [execute database_label migrations] runs all migrations [migrations] on the
      connection pool. *)
  val execute : Label.t -> t list -> (unit, Pool_message.Error.t) Lwt_result.t

  (** [run_all database_label ()] runs all migrations that have been registered on the
      connection pool. *)
  val run_all : Label.t -> unit -> (unit, Pool_message.Error.t) Lwt_result.t

  (** [migrations_status database_label ?migrations ()] returns a list of migration
      namespaces and the number of their unapplied migrations.

      By default, the migrations are checked that have been registered when
      registering the migration service. Custom [migrations] can be provided to
      override this behaviour. *)
  val migrations_status
    :  ?migrations:t list
    -> Label.t
    -> unit
    -> (string * int option) list Lwt.t

  (** [check_migration_status database_label ?migrations ()] returns a list of migration
      namespaces and the number of their unapplied migrations.

      It does the same thing as {!migration_status} and additionally interprets
      whether there are too many, not enough or just the right number of
      migrations applied. If there are too many or not enough migrations
      applied, a descriptive warning message is logged. *)
  val check_migrations_status
    :  ?migrations:t list
    -> Label.t
    -> unit
    -> unit Lwt.t

  (** [pending_migrations database_label ()] returns a list of migrations that need to be
      executed in order to have all migrations applied on the connection pool.
      The returned migration is a tuple [(namespace, number)] where [namespace]
      is the namespace of the migration and [number] is the number of pending
      migrations that need to be applied in order to achieve the desired schema
      version.

      An empty list means that there are no pending migrations and that the
      database schema is up-to-date. *)
  val pending_migrations
    :  ?migrations:t list
    -> Label.t
    -> unit
    -> (string * int) list Lwt.t

  val start : Label.t -> unit -> unit Lwt.t
  val extend_migrations : (string * steps) list -> unit -> (string * steps) list
end

module Pool : sig
  val initialize : ?clear:bool -> unit -> unit Lwt.t
  val create_tested : Label.t -> Url.t -> (t, Pool_message.Error.t) Lwt_result.t

  val create_validated_and_tested
    :  string
    -> string
    -> (t, Pool_message.Error.t) Lwt_result.t

  val connect : Label.t -> (unit, Pool_message.Error.t) Lwt_result.t
  val disconnect : ?error:Caqti_error.t -> Entity.Label.t -> unit Lwt.t

  val all
    :  ?allowed_status:Status.t list
    -> ?exclude:Label.t list
    -> unit
    -> Label.t list

  val is_root : Label.t -> bool

  module Root : sig
    val label : Label.t
    val add : unit -> unit
    val setup : unit -> unit Lwt.t
    val start : unit -> unit Lwt.t
    val stop : unit -> unit Lwt.t
  end

  module Tenant : sig
    val reset : Label.t -> unit Lwt.t
    val add : t -> Label.t
    val drop : Label.t -> unit Lwt.t
    val setup : unit -> Label.t list Lwt.t
    val all : ?status:Status.t list -> unit -> Label.t list
    val find : Label.t -> (t, Pool_message.Error.t) result
    val find_status_by_label : Label.t -> Status.t option

    val find_label_by_url
      :  ?allowed_status:Status.t list
      -> Url.t
      -> (Label.t, Pool_message.Error.t) result

    val update_status : Label.t -> Status.t -> unit Lwt.t
    val set_migration_pending : Label.t list -> unit Lwt.t
    val start : unit -> unit Lwt.t
    val stop : unit -> unit Lwt.t
    val test_connection : Label.t -> (unit, Pool_message.Error.t) Lwt_result.t
  end
end
