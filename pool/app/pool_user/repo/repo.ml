module Paused = Repo_model.Paused
module Disabled = Repo_model.Disabled
module TermsAccepted = Repo_model.TermsAccepted
module Verified = Repo_model.Verified
module CellPhone = Repo_model.CellPhone
module UnverifiedCellPhone = Repo_model.UnverifiedCellPhone
module EmailAddress = Repo_model.EmailAddress
module EmailVerified = Repo_model.EmailVerified
module ImportPending = Repo_model.ImportPending

let user_caqti = Repo_model.User.user_caqti

let select_from_sihl_user_columns =
  {sql|
      LOWER(CONCAT(
        SUBSTR(HEX(user_users.uuid), 1, 8), '-',
        SUBSTR(HEX(user_users.uuid), 9, 4), '-',
        SUBSTR(HEX(user_users.uuid), 13, 4), '-',
        SUBSTR(HEX(user_users.uuid), 17, 4), '-',
        SUBSTR(HEX(user_users.uuid), 21)
      )),
      user_users.email,
      user_users.username,
      user_users.name,
      user_users.given_name,
      user_users.password,
      user_users.status,
      user_users.admin,
      user_users.confirmed,
      user_users.created_at,
      user_users.updated_at,
  |sql}
;;
