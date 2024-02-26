module type Sig = sig
  type status =
    ( (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
      , Caqti_error.load )
      result

  val initialize : unit -> unit
  val fetch_pool : ?ctx:(string * string) list -> unit -> status
  val add_pool : ?required:bool -> ?pool_size:int -> string -> string -> status
  val drop_pool : string -> unit Lwt.t

  val find
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One ]) Caqti_request.t
    -> 'a
    -> ('b, Caqti_error.t) Lwt_result.t

  val find_opt
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
    -> 'a
    -> ('b option, Caqti_error.t) Lwt_result.t

  val collect
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> ('b list, Caqti_error.t) Lwt_result.t

  val exec
    :  ?ctx:(string * string) list
    -> ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> (unit, Caqti_error.t) Lwt_result.t

  val transaction
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> ('a, Caqti_error.t) Lwt_result.t

  val exec_with_connection
    :  ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> (module Caqti_lwt.CONNECTION)
    -> (unit, Caqti_error.t) Lwt_result.t

  val populate
    :  ?ctx:(string * string) list
    -> string
    -> string list
    -> 'a Caqti_type.t
    -> 'a list
    -> (unit, Caqti_error.t) Lwt_result.t
end
