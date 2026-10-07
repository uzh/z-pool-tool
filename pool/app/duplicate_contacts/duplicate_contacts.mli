module Id : sig
  include Pool_model.Base.IdSig
end

module Ignored : sig
  type t

  val value : t -> bool
  val create : bool -> t
end

type t =
  { id : Id.t
  ; contact_a : Contact.t
  ; contact_b : Contact.t
  ; score : float
  ; ignored : Ignored.t
  }

val equal : t -> t -> bool
val show : t -> string

type merge =
  { contact : Contact.t
  ; merged_contact : Contact.t
  ; custom_fields : Custom_field.Public.t list
  }

val find : _ Database.ctx -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val all : ?query:Query.t -> _ Database.ctx -> (t list * Query.t) Lwt.t

val find_by_contact
  :  ?query:Query.t
  -> _ Database.ctx
  -> Contact.t
  -> (t list * Query.t) Lwt.t

val count : _ Database.ctx -> int Lwt.t

(** Contacts eligible for a duplicates check: due (`duplicates_check_due_at` in
    the past), never checked, or last checked more than a week ago. Due
    contacts are returned first. *)
val find_to_check : ?limit:int -> _ Database.ctx -> Contact.t list Lwt.t

val mark_as_checked : _ Database.ctx -> Contact.t -> unit Lwt.t

val merge
  :  _ Database.ctx
  -> ?user_uuid:Pool_common.Id.t
  -> merge
  -> (unit, Pool_message.Error.t) Lwt_result.t

val show_merge : merge -> string
val pp_merge : Format.formatter -> merge -> unit
val equal_merge : merge -> merge -> bool

type event = Ignored of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : _ Database.ctx -> event -> unit Lwt.t
val column_ignore : Query.Column.t
val column_score : Query.Column.t

type hardcoded =
  | Lastname of Pool_user.Lastname.t
  | Firstname of Pool_user.Firstname.t
  | CellPhone of Pool_user.CellPhone.t option
  | Language of Pool_common.Language.t option

val equal_hardcoded : hardcoded -> hardcoded -> bool
val show_hardcoded : hardcoded -> string
val hardcoded_fields : Pool_message.Field.t list
val read_hardcoded : (Pool_message.Field.t * (Contact.t -> hardcoded)) list
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val filterable_by : Query.Filter.Condition.Human.t list option
val default_query : Query.t

module Service : sig
  val run
    :  ?fields:Custom_field.t list
    -> _ Database.ctx
    -> Pool_common.Id.t
    -> unit Lwt.t

  val run_by_tenant : Database.Label.t -> unit Lwt.t
  val register : unit -> Sihl.Container.Service.t
end

module Access : sig
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val read : Id.t -> Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end
