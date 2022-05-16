type public_session =
  { assignment : Assignment.Public.t
  ; session : Session.Public.t
  }

val find_opt_by_experiment
  :  Pool_database.Label.t
  -> Contact.t
  -> Experiment_type.public
  -> public_session option Lwt.t
