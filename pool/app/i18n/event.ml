type t =
  [ `I18n_changed of Entity.property
  | `I18n_created of Entity.property
  | `I18n_destroyed of Entity.property
  ]

type handle_event = t -> unit Lwt.t

let handle_event : handle_event = function
  | `I18n_changed property -> Repo.update property
  | `I18n_created property -> Repo.insert property
  | `I18n_destroyed property -> Repo.delete property
;;
