include Entity
module Repo = Repo

let find_active_user_by_email_opt database_label email =
  let open Utils.Lwt_result.Infix in
  let open Service.User in
  let ctx = Pool_database.to_ctx database_label in
  email
  |> EmailAddress.value
  |> find_by_email_opt ~ctx
  ||> CCFun.flip CCOption.bind (fun ({ status; _ } as user) ->
    match status with
    | Active -> Some user
    | Inactive -> None)
;;

let create_session database_label email ~password =
  let open Utils.Lwt_result.Infix in
  let open Service.User in
  login
    ~ctx:(Pool_database.to_ctx database_label)
    (EmailAddress.value email)
    ~password
  >== fun ({ status; _ } as user) ->
  match status with
  | Inactive -> Error `Does_not_exist
  | Active -> Ok user
;;

module FailedLoginAttempt = struct
  include Login_attempt_entity
  module Repo = Repo_login_attempt
end
