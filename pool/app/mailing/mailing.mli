module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module StartAt : sig
  include Pool_common.Model.BaseSig

  val create : Ptime.t -> (t, Pool_common.Message.error) result
  val value : t -> Ptime.t
  val to_human : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module StartNow : sig
  include Pool_common.Model.BooleanSig
end

module EndAt : sig
  include Pool_common.Model.BaseSig

  val create : Ptime.t -> (t, Pool_common.Message.error) result
  val value : t -> Ptime.t
  val to_human : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Start : sig
  type t =
    | StartNow
    | StartAt of StartAt.t

  val validate : t -> EndAt.t -> (StartAt.t, Pool_common.Message.error) result

  val create
    :  StartAt.t option
    -> StartNow.t
    -> (t, Pool_common.Message.error) result
end

module Rate : sig
  include Pool_common.Model.IntegerSig

  val default : t
end

module Distribution : sig
  module SortableField : sig
    val field : Pool_common.Message.Field.t

    type t =
      | AssignmentCount
      | Firstname
      | InvitationCount
      | Lastname

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
    val to_human : Pool_common.Language.t -> t -> string
    val to_sql : t -> string
    val read : string -> t
    val create : string -> (t, Pool_common.Message.error) result
    val all : t list

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  module SortOrder : sig
    type t =
      | Ascending
      | Descending

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val to_human : Pool_common.Language.t -> t -> string
    val read : string -> t
    val create : string -> (t, Pool_common.Message.error) result
    val all : t list
    val default : t

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type sorted = (SortableField.t * SortOrder.t) list

  val equal_sorted : sorted -> sorted -> bool
  val pp_sorted : Format.formatter -> sorted -> unit
  val show_sorted : sorted -> string
  val sorted_of_yojson : Yojson.Safe.t -> sorted
  val yojson_of_sorted : sorted -> Yojson.Safe.t

  type t =
    | Sorted of sorted
    | Random

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val is_random : t -> bool
  val find_dist : t -> sorted
  val create_sorted : (SortableField.t * SortOrder.t) list -> t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val get_order_element : t -> string

  val schema
    :  unit
    -> ( Pool_common.Message.error
       , sorted )
       Pool_common.Utils.PoolConformist.Field.t

  val is_random_schema
    :  unit
    -> ( Pool_common.Message.error
       , bool )
       Pool_common.Utils.PoolConformist.Field.t

  val of_urlencoded_list
    :  string list
    -> (string, Pool_common.Message.error) result
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
  -> Start.t
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
  | Created of (t * Experiment.Id.t)
  | Updated of (update * t)
  | Deleted of t
  | Stopped of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t * Experiment.Id.t -> event
val updated : update * t -> event
val deleted : t -> event
val stopped : t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_by_experiment
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> t list Lwt.t

val find_overlaps : Pool_database.Label.t -> t -> t list Lwt.t
val find_current : Pool_database.Label.t -> t list Lwt.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Target.t Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Experiment.Id.t -> Guard.ValidationSet.t
    val create : Experiment.Id.t -> Guard.ValidationSet.t
    val read : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val update : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
    val delete : Experiment.Id.t -> Id.t -> Guard.ValidationSet.t
  end
end
