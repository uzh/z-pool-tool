open Utils.Lwt_result.Infix
include Entity
include Event
include Repo
include Pool_user_service

module Password = struct
  include Entity_password

  let validate_current label (user_id : Id.t) password =
    Repo_password.find label user_id
    ||> CCResult.to_opt
    ||> CCOption.map_or ~default:false (CCFun.flip validate password)
  ;;

  let define label (user_id : Id.t) password password_confirmation =
    let* (_ : t) = Repo_password.find label user_id in
    let* password = create password password_confirmation |> Lwt_result.lift in
    let%lwt () = Repo_password.update label (user_id, password) in
    Lwt.return_ok ()
  ;;

  let update = Event.update_password

  module Reset = struct
    let create_token label email =
      let%lwt user = Repo.find_by_email_opt label email in
      match user with
      | Some { id; _ } ->
        let expires_in = Sihl.Time.OneDay in
        Pool_token.create label ~expires_in [ "user_id", Id.value id ]
        |> Lwt.map CCOption.return
      | None ->
        let tags = Database.Logger.Tags.create label in
        Logs.warn (fun m -> m ~tags "No user found with email %a" EmailAddress.pp email);
        Lwt.return_none
    ;;

    let reset_password ~token label password password_confirmation =
      let%lwt user_id = Pool_token.read label token ~k:"user_id" in
      match user_id with
      | None -> Lwt.return_error Pool_message.(Error.Invalid Field.Token)
      | Some user_id -> define label (Id.of_string user_id) password password_confirmation
    ;;

    let lifecycle =
      Sihl.Container.create_lifecycle
        Sihl.Contract.Password_reset.name
        ~dependencies:(fun () -> [ Pool_token.lifecycle; lifecycle ])
    ;;

    let register () = Sihl.Container.Service.create lifecycle
  end
end

let login label email password =
  let open Pool_message in
  match%lwt Repo_password.find_by_email_opt label email with
  | Some hash when Password.validate hash password ->
    find_by_email_exn label email |> Lwt_result.ok
  | Some _ -> Lwt.return_error Error.LoginInvalidEmailPassword
  | None -> Lwt.return_error Error.LoginInvalidEmailPassword
;;

module Web = Pool_user_web

module Repo = struct
  include Repo_entity
  include Repo
end

module FailedLoginAttempt = struct
  include Login_attempt_entity
  module Repo = Repo_login_attempt
end
