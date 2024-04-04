module Guard : Guardian_backend.Pools.Sig

type status =
  ( (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
    , Caqti_error.load )
    result

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

type t

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val create : Label.t -> Url.t -> t
val label : t -> Label.t
val to_ctx : Label.t -> (string * string) list
val of_ctx_opt : (string * string) list -> Label.t option
val of_ctx_exn : (string * string) list -> Label.t

module Repo : sig
  val make_caqti_type
    :  'a Caqti_type.t
    -> ('a -> ('b, Pool_message.Error.t) result)
    -> ('b -> 'a)
    -> 'b Caqti_type.t

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

val fetch_pool : Label.t -> unit -> status
val add_pool : t -> status
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

val test_and_create : Url.t -> Label.t -> (t, Pool_message.Error.t) result Lwt.t

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
