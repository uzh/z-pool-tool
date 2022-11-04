type single_val =
  | Bool of bool
  | Date of Ptime.t
  | Nr of float
  | Option of Custom_field.SelectOption.Id.t
  | Str of string

type value =
  | Single of single_val
  | Lst of single_val list

module Key : sig
  type input_type =
    | Bool
    | Date
    | Nr
    | Str
    | Select of Custom_field.SelectOption.t list

  val show_input_type : input_type -> string

  type hardcoded =
    | Email
    | Name
    | Paused
    | Verified
    | VerifiedAt

  type t =
    | CustomField of Custom_field.Id.t
    | Hardcoded of hardcoded

  type human =
    | CustomField of Custom_field.t
    | Hardcoded of hardcoded

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string
  val hardcoded_to_sql : hardcoded -> string
  val equal_human : human -> human -> bool
  val show_human : human -> string
  val type_of_key : human -> input_type
  val human_to_label : Pool_common.Language.t -> human -> string
  val human_to_value : human -> string
end

module Operator : sig
  type t =
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | NotEqual
    | Like
    | ContainsSome
    | ContainsNone
    | ContainsAll

  val to_sql : t -> string
  val equal : t -> t -> bool
  val show : t -> string
  val input_type_to_operator : Key.input_type -> t list
end

module Predicate : sig
  type t =
    { key : Key.t
    ; operator : Operator.t
    ; value : value
    }

  type human =
    { key : Key.human option
    ; operator : Operator.t option
    ; value : value option
    }

  val create : Key.t -> Operator.t -> value -> t
end

type filter =
  | And of filter list
  | Or of filter list
  | Not of filter
  | Pred of Predicate.t

val show_filter : filter -> string

type t =
  { id : Pool_common.Id.t
  ; filter : filter
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

module Human : sig
  type t =
    | And of t list
    | Or of t list
    | Not of t
    | Pred of Predicate.human

  val of_string : string -> (t, Pool_common.Message.error) result
  val show : t -> string
  val init : ?key:Key.human -> ?operator:Operator.t -> ?value:value -> unit -> t
end

val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> filter -> t
val yojson_of_filter : filter -> Yojson.Safe.t

val filter_of_yojson
  :  Yojson.Safe.t
  -> (filter, Pool_common.Message.error) result

val filter_of_string : string -> (filter, Pool_common.Message.error) result

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

type event =
  | Created of t
  | Updated of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

module Utils : sig
  type filter_label =
    | And
    | Or
    | Not
    | Pred

  val equal_filter_label : filter_label -> filter_label -> bool
  val show_filter_label : filter_label -> string
  val stringify_label : filter_label -> string * string

  val label_of_string
    :  string
    -> (filter_label, Pool_common__Entity_message.error) result

  val all_filter_labels : filter_label list
  val default_filter_label : filter_label
end

module Repo : sig
  val t : t Caqti_type.t
end

val all_keys : Pool_database.Label.t -> Key.human list Lwt.t

val key_of_string
  :  Pool_database.Label.t
  -> string
  -> (Key.human, Pool_common.Message.error) Lwt_result.t

val t_to_human : Key.human list -> filter -> Human.t

val toggle_predicate_type
  :  Human.t
  -> string
  -> (Human.t, Pool_common.Message.error) result
