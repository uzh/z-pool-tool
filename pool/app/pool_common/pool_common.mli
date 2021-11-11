module Message = Entity_message

module Id : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val of_string : string -> t
  val value : t -> string
end

module Language : sig
  type t =
    | En
    | De

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val code : t -> string
  val of_string : string -> (t, Message.error) result
  val t : t Caqti_type.t
  val label : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Database : sig
  module Url : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, Message.error) Result.t
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Label : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val value : t -> string
    val create : string -> (t, Message.error) Result.t
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
  val create : string -> string -> (t, Message.error) Result.t
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
  module Name : sig
    type t

    val value : t -> string
  end

  module Size : sig
    type t

    val value : t -> int
  end

  module Mime : sig
    type t =
      | Css
      | Gif
      | Ico
      | Jpeg
      | Png
      | Svg
      | Webp

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val of_string : string -> (t, Message.error) Result.t
    val to_string : t -> string
    val of_filename : string -> (t, Message.error) Result.t
  end

  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val id : t -> Id.t
  val size : t -> Size.t
  val path : t -> string
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

module Utils : sig
  val schema_decoder
    :  ('a -> ('b, Message.error) result)
    -> Message.field
    -> 'a list
    -> ('b, string) result

  val pool_to_ctx : Database.Label.t -> (string * string) list
  val to_string : Language.t -> Message.t -> string
  val info_to_string : Language.t -> Message.info -> string
  val success_to_string : Language.t -> Message.success -> string
  val warning_to_string : Language.t -> Message.warning -> string
  val error_to_string : Language.t -> Message.error -> string
end
