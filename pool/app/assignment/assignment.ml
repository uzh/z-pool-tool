include Entity
include Event

let find = Repo.find

module Repo = struct
  module Public = struct
    let t = Repo_entity.Public.t
  end
end
