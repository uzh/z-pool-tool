include Entity
include Event

module Service = struct
  include Service
end

let find_pending = Repo.find_pending
