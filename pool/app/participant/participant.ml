include Entity
include Event

let login _ ~email:_ ~password:_ = Utils.todo ()
let find = Repo.find
let insert = Repo.insert
let find_by_user = Utils.todo
let find_duplicates = Utils.todo
