module Id : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val of_string : string -> t
  val value : t -> string
end

module CreatedAt : sig
  type t = Ptime.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val value : 'a -> 'a
end

module UpdatedAt : sig
  type t = Ptime.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val value : 'a -> 'a
end

module Repo : sig
  module Id : sig
    val t : string Caqti_type.t
  end

  module UpdatedAt : sig
    val t : Ptime.t Caqti_type.t
  end

  module CreatedAt : sig
    val t : Ptime.t Caqti_type.t
  end
end
