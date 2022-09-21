module Id : sig
  include Pool_common.Model.IdSig
end

module Model : sig
  type t =
    | Contact
    | Experiment
    | Session

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val all : t list

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Name : sig
  type name

  val equal_name : name -> name -> bool
  val pp_name : Format.formatter -> name -> unit
  val show_name : name -> string
  val name_of_yojson : Yojson.Safe.t -> name
  val yojson_of_name : name -> Yojson.Safe.t
  val value_name : name -> string

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> name
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val find_opt : t -> Pool_common.Language.t -> name option

  val create
    :  (Pool_common.Language.t * string) list
    -> (t, Pool_common.Message.error) result
end

module Hint : sig
  type hint

  val equal_hint : hint -> hint -> bool
  val pp_hint : Format.formatter -> hint -> unit
  val show_hint : hint -> string
  val hint_of_yojson : Yojson.Safe.t -> hint
  val yojson_of_hint : hint -> Yojson.Safe.t
  val value_hint : hint -> string

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> hint
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val find_opt : t -> Pool_common.Language.t -> hint option

  val create
    :  (Pool_common.Language.t * string) list
    -> (t, Pool_common.Message.error) result
end

module FieldType : sig
  type t =
    | Boolean
    | Text

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val all : t list

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Validation : sig
  module Regex : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> string

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  module Error : sig
    type t =
      | Invalid
      | Malformatted
      | NegativeAmount
      | NoValue

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val format_as_label : t -> string
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val all : t list

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type t =
    { regex : Regex.t
    ; error : Error.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Required : sig
  include Pool_common.Model.BooleanSig
end

module Disabled : sig
  include Pool_common.Model.BooleanSig
end

module Admin : sig
  module Hint : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val value : t -> string

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  module Overwrite : sig
    include Pool_common.Model.BooleanSig
  end

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    }
end

type t =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; field_type : FieldType.t
  ; validation : Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; admin : Admin.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val create
  :  ?id:Id.t
  -> Model.t
  -> (Pool_common.Language.t * string) list
  -> (Pool_common.Language.t * string) list
  -> FieldType.t
  -> Validation.t
  -> Required.t
  -> Disabled.t
  -> Admin.t
  -> (t, Pool_common.Message.error) result

val boolean_fields : Pool_common.Message.Field.t list

type event =
  | Created of t
  | Updated of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val find_all : Pool_database.Label.t -> unit -> t list Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Entity.Message.error) result Lwt.t
