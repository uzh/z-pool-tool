include Entity
include Event
include Default
module Guard = Entity_guard

let find = Repo.find
let find_by_key = Repo.find_by_key
let find_all = Repo.find_all
