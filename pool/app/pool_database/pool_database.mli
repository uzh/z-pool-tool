module Url : sig
  type t

  val equal : t -> t -> bool
  val create : string -> (t, Pool_common.Message.error) result

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Label : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t
  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { url : Url.t
  ; label : Label.t
  }

val root : Label.t
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
