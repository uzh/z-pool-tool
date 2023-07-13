module Id : sig
  include Pool_common.Model.IdSig
end

module Model : sig
  type t =
    | Contact
    | Experiment
    | Session

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
end

module Title : Pool_common.Model.StringSig
module Description : Pool_common.Model.StringSig

module Tagged : sig
  type t =
    { model : Model.t
    ; model_uuid : Pool_common.Id.t
    ; tag_uuid : Id.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Model.t -> Id.t -> Id.t -> (t, 'a) result
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> ?description:Description.t
  -> Title.t
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

val find_all : Pool_database.Label.t -> t list Lwt.t
val find_all_models_by_tag_sql : Model.t -> string -> string -> string

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
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
    val assign : ('a -> Guard.ValidationSet.t) -> 'a -> Guard.ValidationSet.t
    val remove : ('a -> Guard.ValidationSet.t) -> 'a -> Guard.ValidationSet.t
  end
end
