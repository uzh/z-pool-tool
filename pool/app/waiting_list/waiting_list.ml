include Entity
include Events
module Guard = Entity_guard
module VersionHistory = Version_history

let find = Repo.find
let user_is_enlisted = Repo.user_is_enlisted
let find_by_experiment = Repo.find_by_experiment
let find_by_contact_and_experiment = Repo.find_by_contact_and_experiment
