include Repo_entity
module Database = Pool_database
module Sql = Repo_sql

let find_by_user = Repo_sql.find_by_user
let find_by_address = Repo_sql.find_by_address
let insert = Repo_sql.insert
let verify = Repo_sql.verify
let delete_unverified_by_user = Repo_sql.delete_unverified_by_user

module Smtp = struct
  let find = Sql.Smtp.find

  let find_by_label label =
    Sql.Smtp.find_by_label
      label
      (label |> Database.Label.value |> Entity.SmtpAuth.Label.of_string)
  ;;

  let find_full_by_label label =
    Sql.Smtp.find_full_by_label
      label
      (label |> Database.Label.value |> Entity.SmtpAuth.Label.of_string)
  ;;

  let insert = Sql.Smtp.insert
  let update = Sql.Smtp.update

  let update_password label Entity.SmtpAuth.{ id; password } =
    Sql.Smtp.update_password label (id, password)
  ;;
end
