type t

val pp : Format.formatter -> t -> unit
val show : t -> string

type create =
  { entity_uuids : Pool_common.Id.t list
  ; message_template : string option
  }

val pp_create : Format.formatter -> create -> unit
val show_create : create -> string
val equal_create : create -> create -> bool

val create
  :  ?message_template:string
  -> entity_uuid:Pool_common.Id.t
  -> Sihl_queue.instance
  -> t

val callback
  :  Pool_database.Label.t
  -> create
  -> Sihl_queue.instance
  -> unit Lwt.t
