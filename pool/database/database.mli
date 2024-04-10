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
  module Data = Caqti_encoders.Data
  module Schema = Caqti_encoders.Schema

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

val root : Label.t
val is_root : Label.t -> bool

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
  val execute : Label.t -> t list -> unit Lwt.t
  val run_all : Label.t -> unit -> unit Lwt.t

  val migrations_status
    :  Label.t
    -> ?migrations:t list
    -> unit
    -> (string * int option) list Lwt.t

  val check_migrations_status
    :  Label.t
    -> ?migrations:t list
    -> unit
    -> unit Lwt.t

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
  val lifecycle : Sihl.Container.lifecycle
  val register : unit -> Sihl.Container.Service.t
end

module Tenant : sig
  val setup_tenant : t -> Label.t Lwt.t
  val setup : unit -> Label.t list Lwt.t
  val find : Label.t -> (t, Pool_message.Error.t) result Lwt.t
  val find_all_running : unit -> Label.t list Lwt.t
  val lifecycle : Sihl.Container.lifecycle
  val register : unit -> Sihl.Container.Service.t
end
