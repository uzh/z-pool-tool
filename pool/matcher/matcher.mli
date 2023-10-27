val find_contacts_by_mailing
  :  Pool_database.Label.t
  -> Mailing.t
  -> int
  -> ( Experiment.t * Contact.t list * Filter.base_condition
       , Pool_common.Message.error )
       Lwt_result.t

val sort_contacts : Contact.t list -> Contact.t list

val events_of_mailings
  :  (Pool_database.Label.t * (Mailing.t * int) list) list
  -> (Pool_database.Label.t, Pool_event.t list) CCPair.t list Lwt.t

val create_invitation_events
  :  Ptime.Span.t
  -> Pool_database.Label.t list
  -> (Pool_database.Label.t * Pool_event.t list) list Lwt.t

val match_invitations : Ptime.Span.t -> Pool_database.Label.t list -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
