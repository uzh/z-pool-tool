module Id : sig
  include Pool_common.Utils.BaseSig

  val create : unit -> t
  val of_string : string -> t
  val value : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module StartAt : sig
  include Pool_common.Utils.BaseSig

  val create : Ptime.t -> (t, Pool_common.Message.error) result
  val value : t -> Ptime.t
  val to_human : t -> string

  val schema
    :  ?field:Pool_common.Message.Field.t
    -> unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module EndAt : sig
  include Pool_common.Utils.BaseSig

  val create : Ptime.t -> (t, Pool_common.Message.error) result
  val value : t -> Ptime.t
  val to_human : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Rate : sig
  include Pool_common.Utils.BaseSig

  val create : int -> (t, Pool_common.Message.error) result
  val value : t -> int
  val default : t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Distribution : sig
  module SortOrder : sig
    type t =
      | Ascending
      | Descending

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val to_human : t -> Pool_common.Language.t -> string
    val read : string -> t
    val create : string -> (t, Pool_common.Message.error) result
    val label : t -> string

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type t = (Pool_common.Message.Field.t * SortOrder.t) list

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : (Pool_common.Message.Field.t * SortOrder.t) list -> t
  val value : t -> (Pool_common.Message.Field.t * SortOrder.t) list
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val get_order_element : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val per_minutes : CCInt.t -> t -> CCFloat.t
val total : t -> int

val create
  :  ?id:Id.t
  -> StartAt.t
  -> EndAt.t
  -> Rate.t
  -> Distribution.t option
  -> (t, Pool_common.Message.error) result

type update =
  { start_at : StartAt.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type event =
  | Created of (t * Pool_common.Id.t)
  | Updated of (update * t)
  | Deleted of t
  | Stopped of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_by_experiment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> t list Lwt.t

val find_overlaps : Pool_database.Label.t -> t -> t list Lwt.t
