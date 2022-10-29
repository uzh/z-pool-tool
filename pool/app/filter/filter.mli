type _ val' =
  | Str : string -> [> `Single ] val'
  | Nr : float -> [> `Single ] val'
  | Bool : bool -> [> `Single ] val'
  | Date : Ptime.t -> [> `Single ] val'
  | Lst : [ `Single ] val' list -> [> `Multi ] val'

module Key : sig
  type input_type =
    | Bool
    | Date
    | Nr
    | Str
    | Select of Custom_field.SelectOption.t list

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
  val equal_human : human -> human -> bool
  val show_human : human -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val type_of_key : human -> input_type
  val human_to_label : Pool_common.Language.t -> human -> string
  val human_to_value : human -> string
end

module Operator : sig
  type _ t =
    | Less : [> `Single ] t
    | LessEqual : [> `Single ] t
    | Greater : [> `Single ] t
    | GreaterEqual : [> `Single ] t
    | Equal : [> `Single ] t
    | NotEqual : [> `Single ] t
    | Like : [> `Single ] t
    | ContainsSome : [> `Multi ] t
    | ContainsNone : [> `Multi ] t
    | ContainsAll : [> `Multi ] t

  val to_string : [> `Multi | `Single ] t -> string
  val to_sql : [> `Multi | `Single ] t -> string
  val equal : [> `Multi | `Single ] t -> [> `Multi | `Single ] t -> bool
end

module Predicate : sig
  type 'a t = Key.t * 'a Operator.t * 'a val'
  type 'a human = Key.human option * 'a Operator.t option * 'a val' option
end

type filter =
  | And of filter * filter
  | Or of filter * filter
  | Not of filter [@printer print_filter "not"]
  (* TODO[timhub]: Fix this type *)
  | PredS of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_s"]
  | PredM of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_m"]

val show_filter : filter -> string

type t =
  { id : Pool_common.Id.t
  ; filter : filter
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

module Human : sig
  type t =
    | And of t * t
    | Or of t * t
    | Not of t
    (* TODO[timhub]: Fix this type *)
    | PredS of [ `Single | `Multi ] Predicate.human
        [@printer print_filter "pred_s"]
    | PredM of [ `Single | `Multi ] Predicate.human
        [@printer print_filter "pred_m"]

  val of_string : string -> (t, Pool_common.Message.error) result
end

val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> filter -> t
val json_to_filter : string -> (filter, Pool_common.Message.error) result
val yojson_of_filter : filter -> Yojson.Safe.t

val filter_of_yojson
  :  Yojson.Safe.t
  -> (filter, Pool_common.Message.error) result

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
    | PredS
    | PredM

  val equal_filter_label : filter_label -> filter_label -> bool
  val show_filter_label : filter_label -> string
  val stringify_label : filter_label -> string * string

  val label_of_string
    :  string
    -> (filter_label, Pool_common__Entity_message.error) result

  val all_filter_labels : filter_label list
  val default_filter_label : filter_label
  val input_type_to_operator : Key.input_type -> [> `Single ] Operator.t list
end

module Repo : sig
  val t : t Caqti_type.t
end

val all_keys : Pool_database.Label.t -> Key.human list Lwt.t

val key_of_string
  :  Pool_database.Label.t
  -> string
  -> (Key.human, Pool_common.Message.error) Lwt_result.t
