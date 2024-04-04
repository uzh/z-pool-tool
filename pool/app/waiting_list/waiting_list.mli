module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module AdminComment : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> string
  val create : string -> t
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

type t =
  { id : Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; admin_comment : AdminComment.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val id : t -> Id.t
val contact : t -> Contact.t
val experiment : t -> Experiment.t
val admin_comment : t -> AdminComment.t option

type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type update = { admin_comment : AdminComment.t option }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

val create
  :  ?id:Pool_common.Id.t
  -> Contact.t
  -> Experiment.t
  -> AdminComment.t option
  -> t

type event =
  | Created of create
  | Updated of update * t
  | Deleted of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t

val find
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val user_is_enlisted
  :  Database.Label.t
  -> Contact.t
  -> Experiment.Id.t
  -> bool Lwt.t

val find_by_experiment
  :  ?query:Query.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t) Lwt.t

val find_by_contact_and_experiment
  :  Database.Label.t
  -> Contact.t
  -> Experiment.Id.t
  -> t option Lwt.t

val column_signed_up_at : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_query : Query.t

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
    val read : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
    val update : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
    val delete : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
  end
end
