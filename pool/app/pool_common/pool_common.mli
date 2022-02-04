module Message = Entity_message
module I18n = Entity_i18n

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

  val schema
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val schema_i18n
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val all : unit -> t list
  val all_codes : unit -> string list
end

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

  module Version : sig
    type t = Version.t

    val t : int Caqti_type.t
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
  module PoolConformist = Pool_common_utils.PoolConformist

  val schema_decoder
    :  (string -> ('b, Message.error) result)
    -> ('b -> string)
    -> Entity_message.field
    -> string
    -> (Message.error, 'b) PoolConformist.Field.t

  val schema_list_decoder
    :  (string list -> ('b list, Message.error) result)
    -> ('b list -> string list)
    -> Entity_message.field
    -> string
    -> (Message.error, 'b list) PoolConformist.Field.t

  val to_string : Language.t -> Message.t -> string
  val info_to_string : Language.t -> Message.info -> string
  val success_to_string : Language.t -> Message.success -> string
  val warning_to_string : Language.t -> Message.warning -> string
  val error_to_string : Language.t -> Message.error -> string
  val field_to_string : Language.t -> Message.field -> string
  val control_to_string : Language.t -> Message.control -> string
  val text_to_string : Language.t -> Entity_i18n.t -> string
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
