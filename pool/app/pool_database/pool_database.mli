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
val create : Label.t -> Url.t -> (t, Pool_common.Message.error) result
val add_pool : t -> unit
val read_pool : t -> Label.t
val of_ctx_opt : (string * string) list -> Label.t option

val test_and_create
  :  Url.t
  -> Label.t
  -> (t, Pool_common.Message.error) Lwt_result.t

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

module GuardBackend : Guardian_backend.Pools.Sig

module Logger : sig
  module Tags : sig
    val create : Label.t -> Logs.Tag.set
    val add : Label.t -> Logs.Tag.set -> Logs.Tag.set
  end
end
