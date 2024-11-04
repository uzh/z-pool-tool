val connect_and_migrate
  :  Database.Label.t
  -> (unit, Pool_message.Error.t) result Lwt.t

type event =
  | Migrated of Database.t
  | StatusUpdated of Database.Label.t * Database.Status.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : 'a -> event -> unit Lwt.t

module Root : sig
  val steps : unit -> Database.Migration.t list
end

module Tenant : sig
  val steps : unit -> Database.Migration.t list
end

val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
