include Entity
include Event

let find = Repo.find
let find_overlapping = Repo.find_overlapping
let query_by_experiment = Repo.query_by_experiment
let find_current_by_experiment db_ctx = Repo.find_by_experiment_and_time `Current db_ctx
let find_upcoming_by_experiment db_ctx = Repo.find_by_experiment_and_time `Upcoming db_ctx
