type _ val' =
  | Str : string -> [> `Single ] val'
  | Nr : float -> [> `Single ] val'
  | Bool : bool -> [> `Single ] val'
  | Date : Ptime.t -> [> `Single ] val'
  | Lst : [ `Single ] val' list -> [> `Multi ] val'

module Key : sig
  type t =
    | Age
    | Birthday
    | Email
    | Name
    | Paused
    | Verified
    | VerifiedAt

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val read : string -> (t, Pool_common.Message.error) result
  val all : t list
  val type_of_key : t -> [> `Bool | `Date | `Nr | `Str ]
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

  val input_type_to_operator
    :  [< `Bool | `Date | `Nr | `Str ]
    -> [> `Single ] Operator.t list
end
