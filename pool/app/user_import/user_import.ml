include Entity
include Event
module Service = Service

let find_pending_by_token = Repo.find_pending_by_token
let find_pending_by_user_id_opt = Repo.find_pending_by_user_id_opt
let find_pending_by_email_opt = Repo.find_pending_by_email_opt
let insert = Repo.insert
let update = Repo.update
let find_contacts_to_notify = Repo.find_contacts_to_notify
let find_contacts_to_remind = Repo.find_contacts_to_remind

module Repo = struct
  let sql_select_columns = Repo.sql_select_columns
  let joins = Repo.joins
  let t = Repo.RepoEntity.t
end
