module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module Text : sig
  type t

  val find_opt : Pool_common.Language.t -> t -> string option
  val find : Pool_common.Language.t -> t -> string

  val create
    :  (Pool_common.Language.t * string) list
    -> (t, Pool_conformist.error_msg) result
end

module StartAt : sig
  include Pool_model.Base.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

module EndAt : sig
  include Pool_model.Base.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

module ShowToAdmins : sig
  include Pool_model.Base.BooleanSig

  val schema
    :  ?default:t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val init : t
end

module ShowToContacts : sig
  include Pool_model.Base.BooleanSig

  val schema
    :  ?default:t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val init : t
end

type t =
  { id : Id.t
  ; text : Text.t
  ; start_at : StartAt.t option
  ; end_at : EndAt.t option
  ; show_to_admins : ShowToAdmins.t
  ; show_to_contacts : ShowToContacts.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib.Sexp.t

val create
  :  ?id:Id.t
  -> Text.t
  -> StartAt.t option
  -> EndAt.t option
  -> ShowToAdmins.t
  -> ShowToContacts.t
  -> t

type admin = t * Pool_tenant.t list

val find
  :  Database.Label.t
  -> Id.t
  -> (t, Pool_message__Pool_message_error.t) result Lwt.t

val all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t

val find_admin
  :  Database.Label.t
  -> Id.t
  -> (admin, Pool_message.Error.t) Lwt_result.t

val find_by_user
  :  Database.Label.t
  -> [< `Admin | `Contact ] * Pool_common.Id.t
  -> t option Lwt.t

val column_start : Query.Column.t
val column_end : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

type event =
  | Created of (t * Pool_tenant.Id.t list)
  | Updated of (t * Pool_tenant.Id.t list)
  | Hidden of (t * Pool_common.Id.t)

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

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
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val read : Id.t -> Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end
