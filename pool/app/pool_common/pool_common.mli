module Message = Entity_message
module I18n = Entity_i18n

module Model : sig
  module Boolean : module type of Entity_base_model.Boolean
  module Integer : module type of Entity_base_model.Integer
  module Ptime : module type of Entity_base_model.Ptime
  module PtimeSpan : module type of Entity_base_model.PtimeSpan
  module SelectorType : module type of Entity_base_model.SelectorType
  module String : module type of Entity_base_model.String

  module type BaseSig = Entity_base_model.BaseSig
  module type BooleanSig = Entity_base_model.BooleanSig
  module type IdSig = Entity_base_model.IdSig
  module type IntegerSig = Entity_base_model.IntegerSig
  module type PtimeSig = Entity_base_model.PtimeSig
  module type PtimeSpanSig = Entity_base_model.PtimeSpanSig
  module type SelectorCoreTypeSig = Entity_base_model.SelectorCoreTypeSig
  module type StringSig = Entity_base_model.StringSig
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

module SortOrder : sig
  type t =
    | Ascending
    | Descending

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val all : t list
  val create : string -> (t, Message.error) result
  val read : string -> t

  val schema
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val default : t
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
      :  ?field:Message.Field.t
      -> unit
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

module VerificationCode : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val of_string : string -> t
  val create : ?length:int -> unit -> t
end

module NotifyVia : sig
  type t =
    | Email
    | TextMessage

  val schema
    :  unit
    -> (Message.error, t) Pool_common_utils.PoolConformist.Field.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val read : string -> t
  val create : string -> (t, Message.error) Result.t
  val all : t list
  val to_human : Language.t -> t -> string
  val checked_by_default : t -> bool
end

module Repo : sig
  val make_caqti_type
    :  'a Caqti_type.t
    -> ('a -> ('b, Message.error) result)
    -> ('b -> 'a)
    -> 'b Caqti_type.t

  module Model : sig
    module SelectorType : module type of Repo.Model.SelectorType
  end

  module Id : sig
    type t = Id.t

    val t : t Caqti_type.t
  end

  module Ptime : sig
    type date = Ptime.date

    val date : date Caqti_type.t
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

  module VerificationCode : sig
    type t = VerificationCode.t

    val t : t Caqti_type.t
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
    val formatted_time : ?with_seconds:bool -> Ptime.t -> string
    val formatted_timespan : Ptime.span -> string
    val timespan_to_minutes : Ptime.span -> string
    val parse_time : string -> (Ptime.t, Message.error) result
    val parse_time_span : string -> (Ptime.Span.t, Message.error) result
    val print_time_span : Ptime.Span.t -> string
    val parse_date_from_calendar : string -> (Ptime.t, Message.error) result
  end

  val schema_decoder
    :  ?tags:Logs.Tag.set
    -> ?default:'b
    -> (string -> ('b, Message.error) result)
    -> ('b -> string)
    -> Message.Field.t
    -> (Message.error, 'b) PoolConformist.Field.t

  val schema_list_decoder
    :  (string list -> ('a, Message.error) result)
    -> ('a -> string list)
    -> Message.Field.t
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
  val bool_to_string : Language.t -> bool -> string

  val with_log_info
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Message.info
    -> Message.info

  val with_log_success
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Message.success
    -> Message.success

  val with_log_warning
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Message.warning
    -> Message.warning

  val with_log_error
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Message.error
    -> Message.error

  val with_log_result_error
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ('a -> Message.error)
    -> ('b, 'a) result
    -> ('b, 'a) result

  val get_or_failwith : ('a, Message.error) result -> 'a
  val failwith : Message.error -> 'a

  val handle_ppx_yojson_err
    :  exn * Yojson.Safe.t
    -> ('a, Entity_message.error) result

  val handle_json_parse_err : string -> ('a, Entity_message.error) result
end
