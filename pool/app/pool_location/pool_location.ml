include Entity
include Event
include Default
module Repo = Repo
module Guard = Entity_guard

let find = Repo.find
let find_all = Repo.find_all
let find_file_storage_blob = Repo_file_mapping.find_storage_blob
