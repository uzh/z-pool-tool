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

val create_tag : Entity.Label.t -> Logs.Tag.set
val fetch_pool : Entity.Label.t -> unit -> status
val add_pool : ?required:bool -> ?pool_size:int -> Entity.t -> status
val drop_pool : Entity.Label.t -> unit Lwt.t

val query
  :  Entity.Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> ('a, Caqti_error.t) Lwt_result.t

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
  :  Entity.Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val transaction_exn
  :  Entity.Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val find_as_transaction
  :  Entity.Label.t
  -> ?setup:(Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> ?cleanup:(Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
  -> 'a Lwt.t

val exec_as_transaction
  :  Entity.Label.t
  -> (Caqti_lwt.connection -> (unit, Caqti_error.t) result Lwt.t) list
  -> unit Lwt.t

val exclude_ids
  :  string
  -> ('a -> string)
  -> Entity.Dynparam.t
  -> 'a list
  -> (Entity.Dynparam.t, string option) CCPair.t

val with_disabled_fk_check
  :  Entity.Label.t
  -> (Caqti_lwt.connection -> ('a, Caqti_error.t) result Lwt.t)
  -> ('a, Caqti_error.t) Lwt_result.t

val clean_requests
  :  Entity.Label.t
  -> (unit, unit, [ `Zero ]) Caqti_request.t list Lwt.t

val clean_all : Entity.Label.t -> (unit, Caqti_error.t) result Lwt.t
val clean_all_exn : Entity.Label.t -> unit Lwt.t
