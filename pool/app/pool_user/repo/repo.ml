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

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"user_users.uuid"
  ; "user_users.email"
  ; "user_users.username"
  ; "user_users.name"
  ; "user_users.given_name"
  ; "user_users.password"
  ; "user_users.status"
  ; "user_users.admin"
  ; "user_users.confirmed"
  ; "user_users.created_at"
  ; "user_users.updated_at"
  ]
;;
