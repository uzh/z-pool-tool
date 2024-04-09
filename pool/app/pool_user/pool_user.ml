include Entity
module CoreRepo = Repo
include Service_user
module Repo = CoreRepo
module PasswordReset = Sihl_user.Password_reset.MakeMariaDb (Pool_token)

let find_active_user_by_email_opt database_label email =
  let open Utils.Lwt_result.Infix in
  email
  |> EmailAddress.value
  |> Persistence.find_by_email_opt database_label
  ||> CCFun.flip CCOption.bind (fun ({ status; _ } as user) ->
    match status with
    | Active -> Some user
    | Inactive -> None)
;;

let create_session database_label email ~password =
  let open Utils.Lwt_result.Infix in
  Persistence.login database_label (EmailAddress.value email) ~password
  >== fun ({ status; _ } as user) ->
  match status with
  | Inactive -> Error `Does_not_exist
  | Active -> Ok user
;;

module FailedLoginAttempt = struct
  include Login_attempt_entity
  module Repo = Repo_login_attempt
end
