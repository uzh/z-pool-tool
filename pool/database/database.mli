module Guard : Guardian_backend.Pools.Sig

type status =
  ( (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
    , Caqti_error.load )
    result

val raise_caqti_error
  :  ?tags:Logs.Tag.set
  -> ( 'a
       , [< `Connect_failed of Caqti_error.connection_error
         | `Connect_rejected of Caqti_error.connection_error
         | `Decode_rejected of Caqti_error.coding_error
         | `Encode_failed of Caqti_error.coding_error
         | `Encode_rejected of Caqti_error.coding_error
         | `Load_failed of Caqti_error.load_error
         | `Load_rejected of Caqti_error.load_error
         | `Post_connect of Caqti_error.call_or_retrieve
         | `Request_failed of Caqti_error.query_error
         | `Response_failed of Caqti_error.query_error
         | `Response_rejected of Caqti_error.query_error
         | `Unsupported
         ] )
       result
  -> 'a

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

module Disabled : sig
  include Pool_model.Base.BooleanSig
end

type t

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val create : ?disabled:Disabled.t -> Label.t -> Url.t -> t
val label : t -> Label.t
val disabled : t -> Disabled.t
val to_ctx : Label.t -> (string * string) list
val of_ctx_opt : (string * string) list -> Label.t option
val of_ctx_exn : (string * string) list -> Label.t

module Repo : sig
  val make_caqti_type
    :  'a Caqti_type.t
    -> ('a -> ('b, Pool_message.Error.t) result)
    -> ('b -> 'a)
    -> 'b Caqti_type.t

  val find : Label.t -> Label.t -> (t, Pool_message.Error.t) result Lwt.t
  val find_all : Label.t -> t list Lwt.t
  val insert : Label.t -> t -> unit Lwt.t
  val update_url : Label.t -> t -> Url.t -> unit Lwt.t
  val update_disabled : Label.t -> t -> Disabled.t -> unit Lwt.t

  module Label : sig
    val t : Label.t Caqti_type.t
  end

  val t : t Caqti_type.t
end

module Logger : sig
  module Tags : sig
    val add_label : string Logs.Tag.def
    val add : Label.t -> Logs.Tag.set -> Logs.Tag.set
    val create : Label.t -> Logs.Tag.set
  end
end

val show_error_with_log
  :  ?tags:Logs.Tag.set
  -> ?log_level:Logs.level
  -> ?msg_prefix:string
  -> [< Caqti_error.t ]
  -> string

val test_and_create : Url.t -> Label.t -> (t, Pool_message.Error.t) result Lwt.t
val fetch_pool : Label.t -> unit -> status
val add_pool : ?required:bool -> ?pool_size:int -> t -> status
val drop_pool : Label.t -> unit Lwt.t

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
  :  Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val transaction_exn
  :  Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val find_as_transaction
  :  Label.t
  -> ?setup:(Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> ?cleanup:(Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val exec_as_transaction
  :  Label.t
  -> (Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> unit Lwt.t

val exclude_ids
  :  string
  -> ('a -> string)
  -> Dynparam.t
  -> 'a list
  -> (Dynparam.t, string option) CCPair.t

val with_disabled_fk_check
  :  Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) result Lwt.t)
  -> ('a, Caqti_error.t) Lwt_result.t

val clean_requests
  :  Label.t
  -> (unit, unit, [ `Zero ]) Caqti_request.t list Lwt.t

val clean_all : Label.t -> (unit, Caqti_error.t) result Lwt.t
val clean_all_exn : Label.t -> unit Lwt.t

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

  (** [register_migrations migrations] registers migrations [migrations] with
      the migration service so it can be executed with `run_all`. *)
  val register_migrations : t list -> unit

  (** [execute database_label migrations] runs all migrations [migrations] on the
      connection pool. *)
  val execute : Label.t -> t list -> unit Lwt.t

  (** [run_all database_label ()] runs all migrations that have been registered on the
      connection pool. *)
  val run_all : Label.t -> unit -> unit Lwt.t

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
  val pending_migrations : Label.t -> unit -> (string * int) list Lwt.t

  val start : Label.t -> unit -> unit Lwt.t
  val extend_migrations : (string * steps) list -> unit -> (string * steps) list

  val run_pending_migrations
    :  Label.t list
    -> (string * steps) list
    -> unit Lwt.t
end

module Root : sig
  val add : unit -> status
  val setup : unit -> status Lwt.t
  val start : unit -> unit Lwt.t
  val stop : unit -> unit Lwt.t
end

val root : Label.t
val is_root : Label.t -> bool

module Tenant : sig
  val setup_tenant : t -> Label.t Lwt.t
  val setup : unit -> Label.t list Lwt.t
  val find : Label.t -> (t, Pool_message.Error.t) result Lwt.t
  val find_all_running : unit -> Label.t list Lwt.t
  val start : unit -> unit Lwt.t
  val stop : unit -> unit Lwt.t
end
