module Id : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val of_string : string -> t
end

module Repo : sig
  module Id : sig
    val t : string Caqti_type.t
  end
end
