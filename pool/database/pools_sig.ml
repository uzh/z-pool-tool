module type ConfigSig = sig
  val database : Entity.t
  val database_pool_size : int
  val expected_databases : int
end

module type Sig = sig
  module Config : ConfigSig

  module Pool : sig
    val initialize : ?additinal_pools:Entity.t list -> unit -> unit
    val add : ?required:bool -> Entity.t -> unit
    val reset : ?required:bool -> Entity.t -> unit Lwt.t
    val drop : Entity.Label.t -> unit Lwt.t
    val clear : unit -> unit

    val fetch
      :  ?retries:int
      -> Entity.Label.t
      -> (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t Lwt.t

    val connect : Entity.Label.t -> (unit, Pool_message.Error.t) result
    val disconnect : ?error:Caqti_error.t -> Entity.Label.t -> unit Lwt.t
    val find : Entity.Label.t -> (Entity.t, Pool_message.Error.t) result
    val find_by_status : ?exclude:string list -> Entity.Status.t list -> Entity.t list

    val find_by_url
      :  ?allowed_status:Entity.Status.t list
      -> Entity.Url.t
      -> (Entity.t, Pool_message.Error.t) result

    val find_all
      :  ?allowed_status:Entity.Status.t list
      -> ?exclude:Entity.Label.t list
      -> unit
      -> Entity.t list

    val raise_caqti_error
      :  Entity.Label.t
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
           Lwt_result.t
      -> 'a Lwt.t
  end

  val query
    :  Entity.Label.t
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val collect
    :  Entity.Label.t
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b list Lwt.t

  val exec : Entity.Label.t -> ('a, unit, [< `Zero ]) Caqti_request.t -> 'a -> unit Lwt.t
  val find : Entity.Label.t -> ('a, 'b, [< `One ]) Caqti_request.t -> 'a -> 'b Lwt.t

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
    -> ?cleanup:(Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> Entity.Label.t
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val transaction_iter
    :  Entity.Label.t
    -> (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> unit Lwt.t
end
