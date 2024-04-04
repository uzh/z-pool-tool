type event = Event.event = Migrated of Database.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : 'a -> event -> unit Lwt.t

module Root : sig
  module Migration = Migration.Root

  val label : Database.Label.t
  val add : unit -> Database.status
  val setup : unit -> Database.status Lwt.t
end

module Tenant : sig
  module Migration = Migration.Tenant

  val label : string

  val setup_tenant
    :  ?run_functions:(?ctx:(string * string) list -> unit -> unit Lwt.t) list
    -> Database.t
    -> Database.Label.t Lwt.t

  val setup
    :  ?run_functions:(?ctx:(string * string) list -> unit -> unit Lwt.t) list
    -> unit
    -> Database.Label.t list Lwt.t
end

val start : unit -> unit Lwt.t
val stop : unit -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
