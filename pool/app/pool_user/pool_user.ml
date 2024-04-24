include Entity
include Service_user

module Repo = struct
  include Repo_entity
  include Repo
end

module PasswordReset = Password_reset

let find_active_user_by_email_opt database_label email =
  let open Utils.Lwt_result.Infix in
  email
  |> find_by_email_opt database_label
  ||> CCFun.flip CCOption.bind (fun ({ status; _ } as user) ->
    match status with
    | Status.Active -> Some user
    | Status.Inactive -> None)
;;

let create_session database_label email password =
  let open Utils.Lwt_result.Infix in
  login database_label email password
  >== fun ({ status; _ } as user) ->
  match status with
  | Status.Inactive -> Error `Does_not_exist
  | Status.Active -> Ok user
;;

module FailedLoginAttempt = struct
  include Login_attempt_entity
  module Repo = Repo_login_attempt
end
