module Id : sig
  include Pool_model.Base.IdSig

  val to_common : t -> Pool_common.Id.t
  val of_common : Pool_common.Id.t -> t
end

module Model : sig
  type t =
    | Contact
    | Experiment

  val field : Pool_message.Field.t
  val min : int
  val max : int
  val to_enum : t -> int
  val of_enum : int -> t option
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val schema : unit -> ('a, t) Pool_conformist.Field.t
  val all : t list
end

module Title : Pool_model.Base.StringSig
module Description : Pool_model.Base.StringSig

module Tagged : sig
  type t =
    { model_uuid : Pool_common.Id.t
    ; tag_uuid : Id.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Pool_common.Id.t -> Id.t -> (t, 'a) result
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  ; model : Model.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> ?description:Description.t
  -> Title.t
  -> Model.t
  -> (t, Pool_message.Error.t) result

module ParticipationTags : sig
  type entity =
    | Experiment of Pool_common.Id.t
    | Session of Pool_common.Id.t

  val get_id : entity -> Pool_common.Id.t
  val find_all : _ Database.ctx -> entity -> t list Lwt.t
  val find_available : _ Database.ctx -> entity -> t list Lwt.t
end

type event =
  | Created of t
  | Updated of (t * t)
  | Tagged of Tagged.t
  | Untagged of Tagged.t
  | ParticipationTagAssigned of ParticipationTags.entity * Id.t
  | ParticipationTagRemoved of ParticipationTags.entity * Id.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t -> event
val updated : t * t -> event
val tagged : Tagged.t -> event
val untagged : Tagged.t -> event
val handle_event : ?user_uuid:Pool_common.Id.t -> _ Database.ctx -> event -> unit Lwt.t
val find : _ Database.ctx -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_multiple : _ Database.ctx -> Id.t list -> (Id.t * Title.t) list Lwt.t

val search_by_title
  :  _ Database.ctx
  -> ?model:Model.t
  -> ?exclude:Id.t list
  -> string
  -> (Id.t * Title.t) list Lwt.t

val find_by : ?query:Query.t -> _ Database.ctx -> (t list * Query.t) Lwt.t
val find_all_with_model : _ Database.ctx -> Model.t -> t list Lwt.t
val find_all_of_entity : _ Database.ctx -> Model.t -> Pool_common.Id.t -> t list Lwt.t

val find_all_validated
  :  ?permission:Guard.Permission.t
  -> _ Database.ctx
  -> Guard.Actor.t
  -> t list Lwt.t

val find_all_validated_with_model
  :  ?permission:Guard.Permission.t
  -> _ Database.ctx
  -> Model.t
  -> Guard.Actor.t
  -> t list Lwt.t

val create_find_all_tag_sql : string -> string -> string

val already_exists
  :  _ Database.ctx
  -> ?exclude_id:Id.t
  -> Title.t
  -> Model.t
  -> bool Lwt.t

val insert : _ Database.ctx -> t -> (unit, Pool_message.Error.t) Lwt_result.t
val update : _ Database.ctx -> t -> unit Lwt.t

val insert_tagged
  :  _ Database.ctx
  -> Tagged.t
  -> (unit, Pool_message.Error.t) Lwt_result.t

val delete_tagged : _ Database.ctx -> Tagged.t -> unit Lwt.t

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
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read_entity : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
    val assign : ('a -> Guard.ValidationSet.t) -> 'a -> Guard.ValidationSet.t
    val remove : ('a -> Guard.ValidationSet.t) -> 'a -> Guard.ValidationSet.t
  end
end

module VersionHistory : Changelog.TSig with type record = t

module RepoEntity : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end

  module Model : sig
    val t : Model.t Caqti_type.t
  end

  val t : t Caqti_type.t
end

val column_title : Query.Column.t
val column_description : Query.Column.t
val column_model : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
