include Entity
include Entity_guard
include Event
module Actor = Actor

let find = Repo.find
let find_by_token = Repo.find_by_token
let all = Repo.all
