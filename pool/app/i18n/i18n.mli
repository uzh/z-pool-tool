module Key : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, Pool_common.Message.error) result

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Content : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, Pool_common.Message.error) result

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t

val create : Key.t -> Pool_common.Language.t -> Content.t -> t

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }

type edit = { content : Content.t }

val id : t -> Pool_common.Id.t
val key : t -> Key.t
val language : t -> Pool_common.Language.t
val content : t -> Content.t

type event =
  | Created of create
  | Updated of t * edit

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t
