val i18n_templates
  :  Pool_database.Label.t
  -> Experiment.t
  -> Pool_common.Language.t list
  -> (Pool_common.Language.t * (I18n.t * I18n.t)) list Lwt.t

val find_contacts_by_mailing
  :  Pool_database.Label.t
  -> Mailing.t
  -> int
  -> (Experiment.t
     * Contact.t list
     * (Pool_common.Language.t * (I18n.t * I18n.t)) list)
     Lwt.t

val match_invitations
  :  ?interval:Sihl.Time.duration
  -> Pool_database.Label.t list
  -> unit Lwt.t

val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
