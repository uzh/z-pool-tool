module Url : sig
  include Pool_model.Base.StringSig
end

module Label : sig
  include Pool_model.Base.StringSig

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
val create : Label.t -> Url.t -> (t, Pool_message.Error.t) result
val add_pool : t -> unit
val drop_pool : Label.t -> unit Lwt.t
val read_pool : t -> Label.t
val to_ctx : Label.t -> (string * string) list
val of_ctx_opt : (string * string) list -> Label.t option
val of_ctx_exn : (string * string) list -> Label.t
val test_and_create : Url.t -> Label.t -> (t, Pool_message.Error.t) Lwt_result.t

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
