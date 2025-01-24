val find_contacts_by_mailing
  :  Database.Label.t
  -> Mailing.t
  -> int
  -> ( Experiment.t * Contact.t list * Filter.base_condition
       , Pool_message.Error.t )
       Lwt_result.t

val sort_contacts : Contact.t list -> Contact.t list
val experiment_has_bookable_spots : Database.Label.t -> Experiment.t -> bool Lwt.t

val events_of_mailings
  :  ?invitation_ids:Pool_common.Id.t list
  -> (Database.Label.t * (Mailing.t * int) list) list
  -> (Database.Label.t, Pool_event.t list) CCPair.t list Lwt.t

val create_invitation_events
  :  ?invitation_ids:Pool_common.Id.t list
  -> Ptime.Span.t
  -> Database.Label.t list
  -> (Database.Label.t * Pool_event.t list) list Lwt.t

val match_invitations : Ptime.Span.t -> Database.Label.t list -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
