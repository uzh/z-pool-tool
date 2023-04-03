include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_by_experiment_and_contact_opt = Repo.find_by_experiment_and_contact_opt
let find_by_session = Repo.find_by_session `All
let find_uncanceled_by_session = Repo.find_by_session `Uncanceled
let find_deleted_by_session = Repo.find_by_session `Deleted
let find_with_follow_ups = Repo.find_with_follow_ups
let find_follow_ups = Repo.find_follow_ups
