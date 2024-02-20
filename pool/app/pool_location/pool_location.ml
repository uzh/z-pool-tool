include Entity
include Event
include Default
module Repo = Repo
module Guard = Entity_guard

let find = Repo.find
let find_all = Repo.find_all
let find_by = Repo.find_by
let find_location_file = Repo_file_mapping.find
let search = Repo.search
let search_multiple_by_id = Repo.search_multiple_by_id
let find_targets_grantable_by_admin = Repo.find_targets_grantable_by_admin

module Statistics = struct
  include Entity_statistics

  let create = Repo_statistics.statistics
end
