module Key : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

module Content : sig
  type t

  val value : t -> string
  val equal : t -> t -> bool
  val create : string -> (t, string) result
  val schema : unit -> ('a, t) Conformist.Field.t
end

(* TODO Hide t*)
type t =
  { id : Pool_common.Id.t
  ; key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

val create : Key.t -> Pool_common.Language.t -> Content.t -> t

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }

type edit = { content : Content.t }

type event =
  | Created of create
  | Edited of t * edit

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t
val translate : Pool_common.Language.t -> string -> string Lwt.t
