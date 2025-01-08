val find_to_warn_about_inactivity
  :  Database.Label.t
  -> Ptime.Span.t list
  -> Contact.t list Lwt.t

module Inactivity : sig
  val handle_disable_contacts
    :  Database.Label.t
    -> Settings.InactiveUser.DisableAfter.t
    -> Ptime.Span.t list
    -> (Email.dispatch list * Contact.event list, Pool_message.Error.t) Lwt_result.t

  val handle_contact_warnings
    :  Database.Label.t
    -> Ptime.Span.t list
    -> (Email.dispatch list * Contact.event list, Pool_message.Error.t) result Lwt.t

  val register : unit -> Sihl.Container.Service.t
end
