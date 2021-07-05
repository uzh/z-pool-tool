type event =
  [ `I18n_changed of Entity.property
  | `I18n_created of Entity.property
  | `I18n_destroyed of Entity.property
  ]

let handle_event : event -> unit Lwt.t = function
  | `I18n_changed property -> Repo.update property
  | `I18n_created property -> Repo.insert property
  | `I18n_destroyed property -> Repo.delete property
;;
