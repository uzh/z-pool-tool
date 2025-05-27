open Entity
module Password = Entity_password

let src = Logs.Src.create "user.event"

type event =
  | Unblocked of t
  | PasswordUpdated of
      Id.t * Password.Plain.t * Password.Plain.t * Password.Confirmation.t [@opaque]
[@@deriving eq, show]

let update_password label user_id ~old_password ~new_password ~new_password_confirmation =
  let open Utils.Lwt_result.Infix in
  let* hashed_password = Repo_password.find label user_id in
  let* new_password' =
    Password.update hashed_password ~old_password ~new_password ~new_password_confirmation
    |> Lwt_result.lift
  in
  let%lwt () = Repo_password.update label (user_id, new_password') in
  Lwt.return_ok ()
;;

let handle_event ?tags pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Unblocked { email; _ } -> Repo_login_attempt.delete_by_email pool email
  | PasswordUpdated (user_id, old_password, new_password, confirmation) ->
    let%lwt (_ : (unit, Pool_message.Error.t) result) =
      update_password
        pool
        user_id
        ~old_password
        ~new_password
        ~new_password_confirmation:confirmation
      >|- Pool_common.Utils.with_log_error ~src ?tags
    in
    Lwt.return_unit
;;
