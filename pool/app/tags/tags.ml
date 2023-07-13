include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let find_all = Repo.find_all
let find_all_models_by_tag_sql = Repo.find_all_models_by_tag_sql
let insert = Repo.insert
let insert_tagged = Repo.insert_tagged
let delete_tagged = Repo.delete_tagged
let update = Repo.update
