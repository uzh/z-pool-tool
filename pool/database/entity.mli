module Dynparam : sig
  type t = Pack : 'a Caqti_type.t * 'a -> t

  val empty : t
  val prefix : 'a Caqti_type.t -> 'a -> t -> t
  val add : 'a Caqti_type.t -> 'a -> t -> t
end

module Url : sig
  include Pool_model.Base.StringSig

  val encrypt : t -> string
  val decrypt : string -> (t, Pool_message.Error.t) result
end

module Label : sig
  include Pool_model.Base.StringSig
end

module Disabled : sig
  include Pool_model.Base.BooleanSig
end

type t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val disabled : t -> Disabled.t
val url : t -> Url.t
val label : t -> Label.t
val create : ?disabled:Disabled.t -> Label.t -> Url.t -> t
val root : Label.t
val is_root : Label.t -> bool

type config =
  { url : string
  ; pool_size : int option
  }

val config : string -> int option -> config
val schema : (string, string -> int option -> config, config) Conformist.t
val pool_size : unit -> int
val database_url : unit -> Url.t

module MariaConfigPool : sig
  val database_pool_size : int
  val database : Pools.connection_type
end

module MariaConfig : sig
  val database_pool_size : int
  val database : Guardian_backend.Pools.connection_type
end

val to_ctx : Label.t -> (string * string) list
val of_ctx_opt : (string * string) list -> Label.t option
val of_ctx_exn : (string * string) list -> Label.t
