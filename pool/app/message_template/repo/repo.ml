include Repo_entity

let insert = Repo_sql.insert
let update = Repo_sql.update
let find = Repo_sql.find

let find_default_by_label_and_language =
  Repo_sql.find_default_by_label_and_language
;;

let find_default_by_label = Repo_sql.find_default_by_label
let find_by_label_to_send = Repo_sql.find_by_label_to_send
let find_all_by_label_to_send = Repo_sql.find_all_by_label_to_send
let all_default = Repo_sql.all_default
let find_all_of_entity_by_label = Repo_sql.find_all_of_entity_by_label
let insert_default_if_not_exists = Repo_sql.insert_default_if_not_exists
let delete = Repo_sql.delete
