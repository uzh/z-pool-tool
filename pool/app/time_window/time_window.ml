include Entity
include Event

let find = Repo.find
let find_overlapping = Repo.find_overlapping
let query_by_experiment = Repo.query_by_experiment
let find_current_by_experiment = Repo.find_current_by_experiment

module Public = struct
  let find_current_by_contact = Repo_public.find_current_by_contact
end
