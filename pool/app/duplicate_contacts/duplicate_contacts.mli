module Id : sig
  include Pool_model.Base.IdSig
end

module Ignored : sig
  type t

  val value : t -> bool
end

type t =
  { id : Id.t
  ; contact_a : Contact.t
  ; contact_b : Contact.t
  ; score : float
  ; ignored : Ignored.t
  }

val run : Database.Label.t -> Pool_common.Id.t -> unit Lwt.t
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t

val find_by_contact
  :  ?query:Query.t
  -> Database.Label.t
  -> Contact.t
  -> (t list * Query.t) Lwt.t

type event = Ignored of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : Database.Label.t -> event -> unit Lwt.t
val column_ignore : Query.Column.t
val column_score : Query.Column.t
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val filterable_by : Query.Filter.Condition.Human.t list option
val default_query : Query.t

module Access : sig
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val read : Id.t -> Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end
