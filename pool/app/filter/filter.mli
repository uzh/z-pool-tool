type _ val' =
  | Str : string -> [> `Single ] val'
  | Nr : float -> [> `Single ] val'
  | Bool : bool -> [> `Single ] val'
  | Date : Ptime.t -> [> `Single ] val'
  | Empty : [> `Single ] val'
  | Lst : [ `Single ] val' list -> [> `Multi ] val'

type key = string

module Operator : sig
  type _ t =
    | Less : [> `Single ] t
    | LessEqual : [> `Single ] t
    | Greater : [> `Single ] t
    | GreaterEqual : [> `Single ] t
    | Equal : [> `Single ] t
    | NotEqual : [> `Single ] t
    | ContainsSome : [> `Multi ] t
    | ContainsNone : [> `Multi ] t
    | ContainsAll : [> `Multi ] t
end

module Predicate : sig
  type 'a t = key * 'a Operator.t * 'a val'
end

type filter =
  | And of filter * filter
  | Or of filter * filter
  | Not of filter [@printer print_filter "not"]
  (* TODO[timhub]: Fix this type *)
  | PredS of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_s"]
  | PredM of [ `Single | `Multi ] Predicate.t [@printer print_filter "pred_m"]

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
val json_to_filter : unit -> (filter, Pool_common.Message.error) result

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

type event = Created of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
