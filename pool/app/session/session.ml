include Entity
include Event

let find_all_for_experiment = Repo.find_all_for_experiment
let find_all_public_for_experiment = Repo.find_all_public_for_experiment
let find = Repo.find
let find_public = Repo.find_public
let find_public_by_assignment = Repo.find_public_by_assignment

module Repo = struct
  module Public = struct
    let t = Repo_entity.Public.t
    let to_entity = Repo_entity.Public.to_entity
    let of_entity = Repo_entity.Public.of_entity
  end
end
