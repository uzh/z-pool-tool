exception Exception of string

val get_or_raise
  :  ?ctx:(string * string) list
  -> ?tags:Logs.Tag.set
  -> ?log_level:Logs.level
  -> ?msg_prefix:string
  -> unit
  -> ('a, [< Caqti_error.t ]) result
  -> 'a

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string * bool) list

module type ConfigSig = sig
  val database : connection_type
  val database_pool_size : int
end

module DefaultConfig : ConfigSig

module type Sig = Database_pools_sig.Sig

module Make : functor (Config : ConfigSig) -> sig
  include Sig

  val transaction_exn
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val query
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> 'a Lwt.t)
    -> ('a, Caqti_error.t) Lwt_result.t

  val query'
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> 'a Lwt.t)
    -> 'a Lwt.t
end
[@@warning "-67"]
