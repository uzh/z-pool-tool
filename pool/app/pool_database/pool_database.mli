module Url : sig
  include Pool_common.Model.StringSig
end

module Label : sig
  include Pool_common.Model.StringSig

  val of_string : string -> t
end

type t =
  { url : Url.t
  ; label : Label.t
  }

val root : Label.t
val is_root : Label.t -> bool
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : string -> string -> (t, Pool_common.Message.error) result
val add_pool : t -> unit
val read_pool : t -> Label.t

module Repo : sig
  module Url : sig
    type t = Url.t

    val t : t Caqti_type.t
  end

  module Label : sig
    type t = Label.t

    val t : t Caqti_type.t
  end

  val t : t Caqti_type.t
end

module GuardBackend : sig
  val main_pool_ref
    : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t option ref

  val pools
    : (string, (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t) Hashtbl.t

  val print_pool_usage : ('a, 'b) Caqti_lwt.Pool.t -> unit

  val connect_or_failwith
    :  ?pool_size:int
    -> ((Caqti_lwt.connection, [> Caqti_error.connect ]) Caqti_lwt.Pool.t -> 'a)
    -> string
    -> 'a

  val add_pool : ?pool_size:int -> string -> string -> unit
  val initialize : unit -> unit

  val fetch_pool
    :  ?ctx:(CCString.t * string) CCList.t
    -> unit
    -> (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

  val transaction
    :  ?ctx:(CCString.t * string) CCList.t
    -> (Caqti_lwt.connection -> 'a)
    -> 'a Lwt.t

  val transaction'
    :  ?ctx:(CCString.t * string) CCList.t
    -> (Caqti_lwt.connection -> ('a, [< Caqti_error.t ]) result)
    -> 'a Lwt.t

  val query
    :  ?ctx:(CCString.t * string) CCList.t
    -> (Caqti_lwt.connection -> 'a Lwt.t)
    -> 'a Lwt.t

  val query'
    :  ?ctx:(CCString.t * string) CCList.t
    -> (Caqti_lwt.connection -> ('a, [< Caqti_error.t ]) result Lwt.t)
    -> 'a Lwt.t

  val find_opt
    :  ?ctx:(CCString.t * string) CCList.t
    -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b option Lwt.t

  val find
    :  ?ctx:(CCString.t * string) CCList.t
    -> ('a, 'b, [< `One ]) Caqti_request.t
    -> 'a
    -> 'b Lwt.t

  val collect
    :  ?ctx:(CCString.t * string) CCList.t
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b list Lwt.t

  val exec
    :  ?ctx:(CCString.t * string) CCList.t
    -> ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> unit Lwt.t
end
