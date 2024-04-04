module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module StartAt : sig
  include Pool_model.Base.BaseSig

  val create : Ptime.t -> (t, Pool_message.Error.t) result
  val create_now : unit -> t
  val value : t -> Ptime.t
  val to_human : t -> string
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module StartNow : sig
  include Pool_model.Base.BooleanSig
end

module EndAt : sig
  include Pool_model.Base.BaseSig

  val create : Ptime.t -> (t, Pool_message.Error.t) result
  val value : t -> Ptime.t
  val to_human : t -> string
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Start : sig
  type t =
    | StartNow
    | StartAt of StartAt.t

  val validate
    :  ?allow_start_in_past:bool
    -> t
    -> EndAt.t
    -> (StartAt.t, Pool_message.Error.t) result

  val create
    :  StartAt.t option
    -> StartNow.t
    -> (t, Pool_message.Error.t) result
end

module Limit : sig
  include Pool_model.Base.IntegerSig

  val default : t
end

module InvitationCount : sig
  include Pool_model.Base.IntegerSig

  val default : t
end

module Distribution : sig
  module SortableField : sig
    val field : Pool_message.Field.t

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
    val create : string -> (t, Pool_message.Error.t) result
    val all : t list
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
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
    val create : string -> (t, Pool_message.Error.t) result
    val all : t list
    val default : t
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
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
  val schema : unit -> (Pool_message.Error.t, sorted) Pool_conformist.Field.t

  val is_random_schema
    :  unit
    -> (Pool_message.Error.t, bool) Pool_conformist.Field.t

  val of_urlencoded_list
    :  string list
    -> (string option, Pool_message.Error.t) result
end

type t =
  { id : Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; limit : Limit.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val per_interval : Ptime.span -> t -> CCFloat.t
val is_past : t -> bool

val create
  :  ?allow_start_in_past:bool
  -> ?id:Id.t
  -> Start.t
  -> EndAt.t
  -> Limit.t
  -> Distribution.t option
  -> (t, Pool_message.Error.t) result

type update =
  { start_at : StartAt.t
  ; end_at : EndAt.t
  ; limit : Limit.t
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
val handle_event : Database.Label.t -> event -> unit Lwt.t
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t

val find_with_detail
  :  Database.Label.t
  -> Id.t
  -> (t * InvitationCount.t, Pool_message.Error.t) Lwt_result.t

val find_by_experiment : Database.Label.t -> Experiment.Id.t -> t list Lwt.t

val find_by_experiment_with_count
  :  Database.Label.t
  -> Query.t option
  -> Experiment.Id.t
  -> ((t * InvitationCount.t) list * Query.t) Lwt.t

val find_overlaps : Database.Label.t -> t -> t list Lwt.t

module Status : sig
  module ToHandle : sig
    include Pool_model.Base.BaseSig

    val value : t -> int
    val create : int -> (t, Pool_message.Error.t) result
  end

  module LastRun : sig
    include Pool_model.Base.BooleanSig
  end

  type status =
    { mailing : t
    ; to_handle : ToHandle.t
    ; last_run : LastRun.t
    }

  type t = status

  val to_handle : t -> ToHandle.t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val find_current : Database.Label.t -> Ptime.span -> t list Lwt.t
end

module Repo : sig
  module Id : sig
    type t = Id.t

    val t : t Caqti_type.t
  end
end

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

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

val column_start : Query.Column.t
val column_end : Query.Column.t
val column_limit : Query.Column.t
val column_invitation_count : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
