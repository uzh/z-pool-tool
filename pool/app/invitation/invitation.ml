include Entity
include Events
module Guard = Entity_guard

let find = Repo.find
let find_by_experiment = Repo.find_by_experiment
let find_by_contact = Repo.find_by_contact
let find_experiment_id_of_invitation = Repo.find_experiment_id_of_invitation

let find_multiple_by_experiment_and_contacts =
  Repo.find_multiple_by_experiment_and_contacts
;;
