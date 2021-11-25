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
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val code : t -> string
  val of_string : string -> (t, Message.error) result
  val t : t Caqti_type.t
  val label : t -> string
  val schema : unit -> ('a, t) Conformist.Field.t
  val all : unit -> t list
  val all_codes : unit -> string list
end

module Database : sig
  module Url : sig
    type t

    val equal : t -> t -> bool
    val create : string -> (t, Message.error) result
    val schema : unit -> ('a, t) Conformist.Field.t
  end

  module Label : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val value : t -> string
    val create : string -> (t, Message.error) result
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
  val create : string -> string -> (t, Message.error) result
  val add_pool : t -> unit
  val read_pool : t -> Label.t
end

module Url : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : string -> (t, Message.error) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module ChangeSet : sig
  module Version : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val value : t -> int
    val create : unit -> t
    val of_int : int -> t
    val increment : t -> t
  end

  type t = (string * Version.t) list

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> (string * Version.t) list
  val create : (string * Version.t) list -> t
  val to_string : t -> string
  val of_string : string -> t
  val empty : t
  val find_version : t -> string -> int option

  val check_for_update
    :  t
    -> t
    -> (string * 'a) list
    -> ((string * 'a) list, Message.error) result
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

    val create : string -> (t, Message.error) result
    val value : t -> string
  end

  module Size : sig
    type t

    val create : int -> (t, Message.error) result
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
    val of_string : string -> (t, Message.error) result
    val to_string : t -> string
    val of_filename : string -> (t, Message.error) result
  end

  type t =
    { id : Id.t
    ; name : Name.t
    ; size : Size.t
    ; mime_type : Mime.t
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }

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

  module Url : sig
    type t = Url.t

    val t : string Caqti_type.t
    val of_pool : Database.Label.t -> t Lwt.t
  end

  module ChangeSet : sig
    module Version : sig
      type t = ChangeSet.Version.t

      val t : int Caqti_type.t
    end
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
  val with_log_info : ?level:Logs.level -> Message.info -> Message.info
  val with_log_success : ?level:Logs.level -> Message.success -> Message.success

  val with_log_warning
    :  ?level:Logs.level
    -> Entity_message.warning
    -> Entity_message.warning

  val with_log_error
    :  ?level:Logs.level
    -> Entity_message.error
    -> Entity_message.error

  val with_log_result_error
    :  ('a -> Message.error)
    -> ('b, 'a) result
    -> ('b, 'a) result
end
