val find_to_warn_about_inactivity
  :  Database.Label.t
  -> Ptime.Span.t list
  -> Contact.t list Lwt.t

module Inactivity : sig
  val register : unit -> Sihl.Container.Service.t
end
