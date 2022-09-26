val count_of_rate
  :  int
  -> Mailing.Rate.t
  -> (int, Pool_common.Message.error) result

val i18n_templates
  :  Pool_database.Label.t
  -> Experiment.t
  -> Pool_common.Language.t list
  -> (Pool_common.Language.t * (I18n.t * I18n.t)) list Lwt.t

val find_contacts_by_mailing
  :  Pool_database.Label.t
  -> ?interval:int
  -> Mailing.t
  -> (Experiment.t
     * Contact.t list
     * Contact.t list
     * (Pool_common.Language.t * (I18n.t * I18n.t)) list)
     Lwt.t
