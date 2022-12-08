open Entity

type create =
  { experiment : Experiment.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type event =
  | Created of Contact.t list * Experiment.t
  | Resent of t
[@@deriving eq, show]

let handle_event pool = function
  | Created (contacts, _) when CCList.is_empty contacts -> Lwt.return_unit
  | Created (contacts, experiment) ->
    Repo.bulk_insert pool contacts experiment.Experiment.id
  | Resent invitation ->
    Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
;;
