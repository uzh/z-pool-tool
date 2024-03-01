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
module Make : functor (_ : ConfigSig) -> Database_pools_sig.Sig
