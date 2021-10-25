module Id : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val of_string : string -> t
  val value : t -> string
end

module Database : sig
  module Url : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, string) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Label : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val value : t -> string
    val create : string -> (t, string) result
    val of_string : string -> t
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  type t =
    { url : Url.t
    ; label : Label.t
    }

  val root : Label.t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : string -> string -> (t, string) result
  val add_pool : t -> unit
  val read_pool : t -> Label.t
end

module CreatedAt : sig
  type t = Ptime.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val value : t -> Ptime.t
end

module UpdatedAt : sig
  type t = Ptime.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val value : t -> Ptime.t
end

module File : sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val id : t -> Id.t
  val filename : t -> string
  val mime : t -> string
  val path : t -> string
  val create : ?id:string -> string -> string -> unit -> t
end

module Repo : sig
  module Id : sig
    type t = Id.t

    val t : string Caqti_type.t
  end

  module Database : sig
    type t = Database.t

    module Url : sig
      type t = Database.Url.t

      val t : t Caqti_type.t
    end

    module Label : sig
      type t = Database.Label.t

      val t : t Caqti_type.t
    end

    val t : t Caqti_type.t
  end

  module CreatedAt : sig
    type t = CreatedAt.t

    val t : t Caqti_type.t
  end

  module UpdatedAt : sig
    type t = UpdatedAt.t

    val t : t Caqti_type.t
  end

  module File : sig
    type t = File.t

    val t : t Caqti_type.t
  end
end
