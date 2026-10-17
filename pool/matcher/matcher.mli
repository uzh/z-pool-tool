val find_contacts_by_mailing
  :  _ Database.ctx
  -> Mailing.t
  -> int
  -> ( Experiment.t * Contact.t list * Filter.base_condition
       , Pool_message.Error.t )
       Lwt_result.t

val sort_contacts : Contact.t list -> Contact.t list
val experiment_has_bookable_spots : _ Database.ctx -> Experiment.t -> bool Lwt.t

val events_of_mailings
  :  ?invitation_ids:Pool_common.Id.t list
  -> _ Database.ctx
  -> (Mailing.t * int) list
  -> Pool_event.t list Lwt.t

val create_invitation_events
  :  ?invitation_ids:Pool_common.Id.t list
  -> Ptime.Span.t
  -> _ Database.ctx
  -> Pool_event.t list Lwt.t

val match_invitations : Ptime.Span.t -> _ Database.ctx -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
