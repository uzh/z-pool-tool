open Utils.Lwt_result.Infix
open Entity
open Repo

let log_src = Logs.Src.create "pool.user"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let find_active_by_email_opt database_label email =
  find_by_email_opt database_label email
  ||> CCFun.flip CCOption.bind (fun ({ status; _ } as user) ->
    match status with
    | Status.Active -> Some user
    | Status.Inactive -> None)
;;

let update ?email ?lastname ?firstname ?status ?confirmed label user =
  let%lwt () =
    Entity.update ?email ?lastname ?firstname ?status ?confirmed user |> Repo.update label
  in
  find_exn label user.id
;;

let confirm label user =
  let%lwt () = confirm user |> Repo.update label in
  find_exn label user.id
;;

let create
      ?id
      ?admin
      ?confirmed
      label
      email
      lastname
      firstname
      password
      password_confirmation
  =
  let* user = create ?id ?admin ?confirmed email lastname firstname |> Lwt_result.lift in
  let* password =
    Entity_password.create password password_confirmation |> Lwt_result.lift
  in
  let%lwt () = Repo.insert label (user, password) in
  let%lwt user = find_exn label user.id in
  Lwt.return_ok user
;;

let validate_existance label email =
  Repo.find_by_email_opt label email
  |> Lwt.map (function
    | Some _ -> Error Pool_message.(Error.AlreadyExisting Field.User)
    | None -> Ok ())
;;

let create_user ?id label email lastname firstname password password_confirmation =
  let* () = validate_existance label email in
  create ?id label email lastname firstname password password_confirmation
;;

let create_user_unvalidated ?id label email lastname firstname password =
  let* () = validate_existance label email in
  Entity_password.to_confirmed password
  |> create ?id label email lastname firstname password
;;

let create_admin ?id label email lastname firstname password password_confirmation =
  let* () = validate_existance label email in
  create
    ?id
    ~admin:(IsAdmin.create true)
    ~confirmed:(Confirmed.create true)
    label
    email
    lastname
    firstname
    password
    password_confirmation
;;

let lifecycle =
  Sihl.Container.create_lifecycle "pool_user_service" ~dependencies:(fun () ->
    [ Pool_database.lifecycle ])
;;

let register ?(commands = []) () =
  Repo.register_migration ();
  Sihl.Container.Service.create ~commands lifecycle
;;

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
