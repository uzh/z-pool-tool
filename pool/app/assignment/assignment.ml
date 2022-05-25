include Entity
include Event

let find = Repo.find
let find_by_experiment_and_contact_opt = Repo.find_by_experiment_and_contact_opt
let find_by_session = Repo.find_by_session `All
let find_uncanceled_by_session = Repo.find_by_session `Uncanceled
