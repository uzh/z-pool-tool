include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let all = Repo.all

module Repo = struct
  module Id = struct
    let t = Pool_common.Repo.Id.t
  end

  let t = Repo.t
end
