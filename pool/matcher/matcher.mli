val find_contacts_by_mailing
  :  Pool_database.Label.t
  -> Mailing.t
  -> int
  -> (Experiment.t * Contact.t list, Pool_common.Message.error) Lwt_result.t

val match_invitations
  :  ?interval:Sihl.Time.duration
  -> Pool_database.Label.t list
  -> unit Lwt.t

val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
