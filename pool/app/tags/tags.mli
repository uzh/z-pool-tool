module Id : sig
  include Pool_common.Model.IdSig

  val to_common : t -> Pool_common.Id.t
  val of_common : Pool_common.Id.t -> t
end

module Model : sig
  type t =
    | Contact
    | Experiment

  val field : Pool_common.Message.Field.t
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
  val schema : unit -> ('a, t) Pool_common.Utils.PoolConformist.Field.t
  val all : t list
end

module Title : Pool_common.Model.StringSig
module Description : Pool_common.Model.StringSig

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
  -> (t, Pool_common.Message.error) result

type event =
  | Created of t
  | Updated of t
  | Tagged of Tagged.t
  | Untagged of Tagged.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t -> event
val updated : t -> event
val tagged : Tagged.t -> event
val untagged : Tagged.t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_multiple
  :  Pool_database.Label.t
  -> Id.t list
  -> (Id.t * Title.t) list Lwt.t

val search_by_title
  :  Pool_database.Label.t
  -> ?model:Model.t
  -> ?exclude:Id.t list
  -> string
  -> (Id.t * Title.t) list Lwt.t

val find_all : Pool_database.Label.t -> t list Lwt.t
val find_all_with_model : Pool_database.Label.t -> Model.t -> t list Lwt.t

val find_all_of_entity
  :  Pool_database.Label.t
  -> Model.t
  -> Pool_common.Id.t
  -> t list Lwt.t

val find_all_validated
  :  ?action:Guard.Action.t
  -> Pool_database.Label.t
  -> 'a Guard.Actor.t
  -> t list Lwt.t

val find_all_validated_with_model
  :  ?action:Guard.Action.t
  -> Pool_database.Label.t
  -> Model.t
  -> 'a Guard.Actor.t
  -> t list Lwt.t

val create_find_all_tag_sql : string -> string -> string

val already_exists
  :  Pool_database.Label.t
  -> ?exclude_id:Id.t
  -> Title.t
  -> Model.t
  -> bool Lwt.t

val insert
  :  Pool_database.Label.t
  -> t
  -> (unit, Pool_common.Message.error) result Lwt.t

val update : Pool_database.Label.t -> t -> unit Lwt.t

val insert_tagged
  :  Pool_database.Label.t
  -> Tagged.t
  -> (unit, Pool_common.Message.error) result Lwt.t

val delete_tagged : Pool_database.Label.t -> Tagged.t -> unit Lwt.t

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
