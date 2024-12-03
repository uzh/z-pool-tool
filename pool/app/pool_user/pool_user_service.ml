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
    Entity.update ?email ?lastname ?firstname ?status ?confirmed user
    |> Repo.update label
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
  let* user =
    create ?id ?admin ?confirmed email lastname firstname |> Lwt_result.lift
  in
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

let create_user
      ?id
      label
      email
      lastname
      firstname
      password
      password_confirmation
  =
  let* () = validate_existance label email in
  create ?id label email lastname firstname password password_confirmation
;;

let create_user_unvalidated ?id label email lastname firstname password =
  let* () = validate_existance label email in
  Entity_password.to_confirmed password
  |> create ?id label email lastname firstname password
;;

let create_admin
      ?id
      label
      email
      lastname
      firstname
      password
      password_confirmation
  =
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
