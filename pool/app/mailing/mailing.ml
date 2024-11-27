include Entity
include Event
module Guard = Entity_guard
module VersionHistory = Version_history

let find = Repo.find
let find_with_detail = Repo.find_with_detail
let find_by_experiment = Repo.find_by_experiment
let find_by_experiment_with_count = Repo.find_by_experiment_with_count
let find_overlaps = Repo.find_overlaps

module Status = struct
  include Entity_status

  let find_current = Repo.Status.find_current
end

module Repo = struct
  module Id = Repo_entity.Id
end
