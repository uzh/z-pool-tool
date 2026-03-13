include Repo_entity
module Database = Database
module Sql = Repo_sql

let find_by_user = Repo_sql.find_by_user
let find_by_address = Repo_sql.find_by_address
let insert = Repo_sql.insert
let verify = Repo_sql.verify
let delete_unverified_by_user = Repo_sql.delete_unverified_by_user

module Smtp = struct
  let find = Sql.Smtp.find
  let find_by_label = Sql.Smtp.find_by_label
  let find_by = Sql.Smtp.find_by
  let find_full = Sql.Smtp.find_full
  let find_full_default = Sql.Smtp.find_full_default
  let find_default = Sql.Smtp.find_default
  let find_default_opt = Sql.Smtp.find_default_opt
  let find_all = Sql.Smtp.find_all
  let find_for_experiment = Sql.Smtp.find_for_experiment
  let insert = Sql.Smtp.insert
  let update = Sql.Smtp.update
  let delete = Sql.Smtp.delete

  let update_password label Entity.SmtpAuth.{ id; password } =
    Sql.Smtp.update_password label (id, password)
  ;;

  let count_invitations_sent_since = Sql.Smtp.count_invitations_sent_since

  module RepoEntity = Repo_entity_smtp_auth
end
