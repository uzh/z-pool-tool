module Message = Entity_message
module I18n = Entity_i18n

module Model : sig
  module type IdSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : unit -> t
    val of_string : string -> t
    val value : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t

    val schema
      :  ?field:Entity_message.Field.t
      -> unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module Boolean : sig
    type t = bool

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : bool -> t
    val value : t -> bool
    val stringify : t -> string
    val of_string : string -> t

    val schema
      :  Entity_message.Field.t
      -> unit
      -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type BooleanSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : bool -> t
    val value : t -> bool
    val stringify : t -> string
    val of_string : string -> t

    val schema
      :  unit
      -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module String : sig
    type t = string

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : string -> (t, Entity_message.error) result
    val value : t -> string
    val of_string : string -> t

    val schema
      :  Entity_message.Field.t
      -> ?validation:(t -> (t, Entity_message.error) result)
      -> unit
      -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type StringSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : string -> (t, Entity_message.error) result
    val value : t -> string
    val of_string : string -> t

    val schema
      :  unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module Integer : sig
    type t = int

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> int

    val schema
      :  Entity_message.Field.t
      -> (int -> (t, Entity_message.error) result)
      -> unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type IntegerSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val create : int -> (t, Entity_message.error) result
    val value : t -> int

    val schema
      :  unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module PtimeSpan : sig
    type t = Ptime.Span.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> Ptime.Span.t
    val to_human : t -> string

    val schema
      :  Entity_message.Field.t
      -> (Ptime.Span.t -> (t, Entity_message.error) result)
      -> unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type PtimeSpanSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> Ptime.Span.t
    val to_human : t -> string

    val schema
      :  unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module Ptime : sig
    type t = Ptime.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> Ptime.t
    val create_now : unit -> t
    val to_human : t -> string

    val schema
      :  Entity_message.Field.t
      -> (Ptime.t -> (t, Entity_message.error) result)
      -> unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type PtimeSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> Ptime.t
    val create_now : unit -> t
    val to_human : t -> string

    val schema
      :  unit
      -> (Entity_message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module type BaseSig = sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, Entity_message.error) result

    val schema
      :  unit
      -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t
  end
end

module Id : Model.IdSig

module Language : sig
  type t =
    | En
    | De

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : string -> (t, Message.error) result
  val label : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  val schema
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val all : t list
  val all_codes : string list
  val field_of_t : t -> Message.Field.t
end

module Version : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
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
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

module UpdatedAt : sig
  type t = Ptime.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : unit -> t
  val value : t -> Ptime.t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
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
      | Pdf
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
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

module Reminder : sig
  module LeadTime : sig
    type t

    val equal : t -> t -> bool
    val show : t -> string
    val create : Ptime.Span.t -> (t, Message.error) result
    val value : t -> Ptime.Span.t
    val pp : Format.formatter -> t -> unit
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t

    val schema
      :  unit
      -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t
  end

  module SentAt : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : Ptime.t -> t
    val create_now : unit -> t
    val value : t -> Ptime.t
  end
end

module ExperimentType : sig
  type t =
    | Lab
    | Online

  val schema
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val read : string -> t
  val all : t list
end

module Repo : sig
  module Id : sig
    type t = Id.t

    val t : t Caqti_type.t
  end

  module Language : sig
    type t = Language.t

    val t : t Caqti_type.t
  end

  module Version : sig
    type t = Version.t

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

  module Reminder : sig
    module LeadTime : sig
      type t = Reminder.LeadTime.t

      val t : t Caqti_type.t
    end

    module SentAt : sig
      type t = Reminder.SentAt.t

      val t : t Caqti_type.t
    end
  end

  module ExperimentType : sig
    type t = ExperimentType.t

    val t : t Caqti_type.t
  end
end

module Utils : sig
  module PoolConformist = Pool_common_utils.PoolConformist

  module Time : sig
    val ptime_to_sexp : Ptime.t -> Sexplib0.Sexp.t
    val formatted_date_time : Ptime.t -> string
    val formatted_date : Ptime.t -> string
    val formatted_time : Ptime.t -> string
    val formatted_timespan : Ptime.span -> string
    val timespan_to_hours : Ptime.span -> string
    val parse_time : string -> (Ptime.t, Message.error) result
    val parse_time_span : string -> (Ptime.Span.t, Message.error) result
    val print_time_span : Ptime.Span.t -> string
  end

  val schema_decoder
    :  ?default:'b
    -> (string -> ('b, Message.error) result)
    -> ('b -> string)
    -> Entity_message.Field.t
    -> (Message.error, 'b) PoolConformist.Field.t

  val schema_list_decoder
    :  (string list -> ('a, Entity_message.error) result)
    -> ('a -> string list)
    -> Entity_message.Field.t
    -> ('b, 'a) PoolConformist.Field.t

  val to_string : Language.t -> Message.t -> string
  val info_to_string : Language.t -> Message.info -> string
  val success_to_string : Language.t -> Message.success -> string
  val warning_to_string : Language.t -> Message.warning -> string
  val error_to_string : Language.t -> Message.error -> string
  val field_to_string : Language.t -> Message.Field.t -> string
  val field_to_string_capitalized : Language.t -> Message.Field.t -> string
  val control_to_string : Language.t -> Message.control -> string
  val confirmable_to_string : Language.t -> I18n.confirmable -> string
  val text_to_string : Language.t -> Entity_i18n.t -> string
  val nav_link_to_string : Language.t -> Entity_i18n.nav_link -> string
  val hint_to_string : Language.t -> Entity_i18n.hint -> string
  val with_log_info : ?level:Logs.level -> Message.info -> Message.info
  val with_log_success : ?level:Logs.level -> Message.success -> Message.success
  val bool_to_string : Language.t -> bool -> string

  val with_log_warning
    :  ?level:Logs.level
    -> Entity_message.warning
    -> Entity_message.warning

  val with_log_error
    :  ?level:Logs.level
    -> ?tags:Logs.Tag.set
    -> Entity_message.error
    -> Entity_message.error

  val with_log_result_error
    :  tags:Logs.Tag.set
    -> ('a -> Message.error)
    -> ('b, 'a) result
    -> ('b, 'a) result

  val get_or_failwith : ('a, Message.error) result -> 'a
end
