val find_to_warn_about_inactivity
  :  Database.Label.t
  -> Ptime.Span.t list
  -> Contact.t list Lwt.t

module Inactivity : sig
  val register : unit -> Sihl.Container.Service.t
end

type event = NotifiedAbountInactivity of Contact.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
