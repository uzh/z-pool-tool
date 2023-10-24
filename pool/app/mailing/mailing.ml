include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_by_experiment = Repo.find_by_experiment
let find_overlaps = Repo.find_overlaps
let find_current = Repo.find_current

module Repo = struct
  module Id = Repo_entity.Id
end
