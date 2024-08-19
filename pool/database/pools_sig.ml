module type ConfigSig = sig
  val database : Entity.t
  val database_pool_size : int
  val expected_databases : int
end

module type Sig = sig
  module Config : ConfigSig

  val initialize : ?additinal_pools:Entity.t list -> unit -> unit
  val add_pool : ?required:bool -> Entity.t -> unit
  val drop_pool : Entity.Label.t -> unit Lwt.t
  val connect : Entity.Label.t -> (unit, Pool_message.Error.t) result

  val query
    :  Entity.Label.t
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val collect
    :  Entity.Label.t
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b list Lwt.t

  val exec
    :  Entity.Label.t
    -> ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> unit Lwt.t

  val find
    :  Entity.Label.t
    -> ('a, 'b, [< `One ]) Caqti_request.t
    -> 'a
    -> 'b Lwt.t

  val find_opt
    :  Entity.Label.t
    -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b option Lwt.t

  val populate
    :  Entity.Label.t
    -> string
    -> string list
    -> 'a Caqti_type.t
    -> 'a list
    -> unit Lwt.t

  val transaction
    :  ?setup:(Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> ?cleanup:
         (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> Entity.Label.t
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val transaction_iter
    :  Entity.Label.t
    -> (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> unit Lwt.t
end
