module I18n = Entity_i18n
module Id : Pool_model.Base.IdSig

module Language : sig
  type t =
    | En
    | De

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val create : string -> (t, Pool_message.Error.t) result
  val label : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val compare : t -> t -> int
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val all : t list
  val all_codes : string list
  val field_of_t : t -> Pool_message.Field.t
  val read_opt : string -> t option
end

module Version : sig
  include Pool_model.Base.IntegerSig

  val create : unit -> t
  val increment : t -> t
end

module CreatedAt : sig
  include Pool_model.Base.PtimeSig
end

module UpdatedAt : sig
  include Pool_model.Base.PtimeSig
end

module File : sig
  module Name : sig
    type t

    val create : string -> (t, Pool_message.Error.t) result
    val value : t -> string
  end

  module Size : sig
    type t

    val create : int -> (t, Pool_message.Error.t) result
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
    val of_string : string -> (t, Pool_message.Error.t) result
    val to_string : t -> string
    val of_filename : string -> (t, Pool_message.Error.t) result
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
  val externalized_path : t -> string
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
  val create : string -> (t, Pool_message.Error.t) result
  val read : string -> t
  val flip : t -> t
  val to_query_parts : t -> (Pool_message.Field.t * string) list
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val default : t
end

module MessageChannel : sig
  type t =
    | Email
    | TextMessage

  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val read : string -> t
  val create : string -> (t, Pool_message.Error.t) Result.t
  val all : t list
  val filtered_channels : bool -> t list
end

module Reminder : sig
  module EmailLeadTime : sig
    include Pool_model.Base.DurationSig
  end

  module TextMessageLeadTime : sig
    include Pool_model.Base.DurationSig
  end

  module SentAt : sig
    include Pool_model.Base.PtimeSig
  end
end

module ExperimentType : sig
  type t =
    | Lab
    | Online

  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val read : string -> t
  val all : t list
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module VerificationCode : sig
  include Pool_model.Base.StringSig

  val create : ?length:int -> unit -> t
end

module NotifyVia : sig
  type t =
    | Email
    | TextMessage

  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val read : string -> t
  val create : string -> (t, Pool_message.Error.t) Result.t
  val all : t list
  val to_human : Language.t -> t -> string
  val checked_by_default : t -> bool
end

module NotifyContact : sig
  include Pool_model.Base.BooleanSig
end

module Repo : sig
  val make_caqti_type
    :  'a Caqti_type.t
    -> ('a -> ('b, Pool_message.Error.t) result)
    -> ('b -> 'a)
    -> 'b Caqti_type.t

  val encode_yojson : ('a -> Yojson.Safe.t) -> 'a -> (string, 'b) result

  val decode_yojson
    :  (Yojson.Safe.t -> 'a)
    -> Pool_message.Field.t
    -> string
    -> ('a, string) result

  module Model : sig
    module SelectorType : module type of Repo.Model.SelectorType
  end

  module Id : Pool_model.Base.CaqtiSig with type t = Id.t

  module Ptime : sig
    type date = Ptime.date

    val date : date Caqti_type.t
  end

  module Language : Pool_model.Base.CaqtiSig with type t = Language.t
  module Version : Pool_model.Base.CaqtiSig with type t = Version.t
  module CreatedAt : Pool_model.Base.CaqtiSig with type t = CreatedAt.t
  module UpdatedAt : Pool_model.Base.CaqtiSig with type t = UpdatedAt.t
  module File : Pool_model.Base.CaqtiSig with type t = File.t

  module Reminder : sig
    module EmailLeadTime :
      Pool_model.Base.CaqtiSig with type t = Reminder.EmailLeadTime.t

    module TextMessageLeadTime :
      Pool_model.Base.CaqtiSig with type t = Reminder.TextMessageLeadTime.t

    module SentAt : Pool_model.Base.CaqtiSig with type t = Reminder.SentAt.t
  end

  module VerificationCode :
    Pool_model.Base.CaqtiSig with type t = VerificationCode.t

  module ExperimentType :
    Pool_model.Base.CaqtiSig with type t = ExperimentType.t
end

module Utils : sig
  val to_string : Language.t -> Pool_message.t -> string
  val info_to_string : Language.t -> Pool_message.Info.t -> string
  val success_to_string : Language.t -> Pool_message.Success.t -> string
  val warning_to_string : Language.t -> Pool_message.Warning.t -> string
  val error_to_string : Language.t -> Pool_message.Error.t -> string
  val field_to_string : Language.t -> Pool_message.Field.t -> string
  val field_to_string_capitalized : Language.t -> Pool_message.Field.t -> string
  val control_to_string : Language.t -> Pool_message.Control.t -> string
  val confirmable_to_string : Language.t -> I18n.confirmable -> string
  val text_to_string : Language.t -> Entity_i18n.t -> string
  val nav_link_to_string : Language.t -> Entity_i18n.nav_link -> string
  val hint_to_string : Language.t -> Entity_i18n.hint -> string
  val bool_to_string : Language.t -> bool -> string

  val with_log_info
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Pool_message.Info.t
    -> Pool_message.Info.t

  val with_log_success
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Pool_message.Success.t
    -> Pool_message.Success.t

  val with_log_warning
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Pool_message.Warning.t
    -> Pool_message.Warning.t

  val with_log_error
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ?level:Logs.level
    -> Pool_message.Error.t
    -> Pool_message.Error.t

  val with_log_result_error
    :  ?src:Logs.Src.t
    -> ?tags:Logs.Tag.set
    -> ('a -> Pool_message.Error.t)
    -> ('b, 'a) result
    -> ('b, 'a) result

  val get_or_failwith : ('a, Pool_message.Error.t) result -> 'a
  val failwith : Pool_message.Error.t -> 'a
end
