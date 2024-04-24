let log_src = Logs.Src.create "pool.user"

module Logs = (val Logs.src_log log_src : Logs.LOG)

exception Exception of string

let find_opt = Repo.find_opt

let find label user_id =
  let%lwt m_user = find_opt label user_id in
  match m_user with
  | Some user -> Lwt.return user
  | None ->
    Logs.err (fun m -> m "User not found with id %a" Entity.Id.pp user_id);
    raise (Exception "User not found")
;;

let find_by_email_opt = Repo.get_by_email

let find_by_email label email =
  let%lwt user = find_by_email_opt label email in
  match user with
  | Some user -> Lwt.return user
  | None ->
    Logs.err (fun m ->
      m "User not found with email %a" Entity.EmailAddress.pp email);
    raise (Exception "User not found")
;;

let update ?email ?name ?given_name ?status label user =
  let updated =
    { user with
      Entity.email = CCOption.value ~default:user.Entity.email email
    ; name = CCOption.value ~default:user.Entity.name name
    ; given_name = CCOption.value ~default:user.Entity.given_name given_name
    ; status = Option.value ~default:user.Entity.status status
    }
  in
  let%lwt () = Repo.update label updated in
  find label user.Entity.id
;;

let update_password
  label
  user
  ~old_password
  ~new_password
  ~new_password_confirmation
  =
  let open Utils.Lwt_result.Infix in
  let* () =
    Entity.validate_change_password
      user
      ~old_password
      ~new_password
      ~new_password_confirmation
    |> Lwt_result.lift
  in
  let* updated_user =
    Entity.set_user_password user new_password |> Lwt_result.lift
  in
  let%lwt () = Repo.update label updated_user in
  find label user.Entity.id |> Lwt.map Result.ok
;;

let set_password label user password password_confirmation =
  let%lwt result =
    Entity.Password.validate_password_confirmation
      password
      password_confirmation
    |> Lwt.return
  in
  match result with
  | Error msg -> Lwt.return @@ Error msg
  | Ok () ->
    let%lwt result = Repo.find_opt label user.Entity.id in
    (* Re-fetch user to make sure that we have an up-to-date model *)
    let%lwt user =
      match result with
      | Some user -> Lwt.return user
      | None -> raise (Exception "Failed to create user")
    in
    let updated_user =
      match Entity.set_user_password user password with
      | Ok user -> user
      | Error msg ->
        let msg = Pool_message.Error.show msg in
        Logs.err (fun m ->
          m
            "Can not set password of user %a: %s"
            Entity.EmailAddress.pp
            user.Entity.email
            msg);
        raise (Exception msg)
    in
    let%lwt () = Repo.update label updated_user in
    find label user.Entity.id |> Lwt.map Result.ok
;;

let create ?id ?admin ?confirmed label email name given_name password =
  let user =
    Entity.create ?id ?admin ?confirmed email name given_name password
  in
  match user with
  | Ok user ->
    let%lwt () = Repo.insert label user in
    let%lwt user = find label user.Entity.id in
    Lwt.return_ok user
  | Error _ -> Lwt.return_error Pool_message.(Error.NotFound Field.User)
;;

let create_user ?id label email name given_name password =
  let%lwt user = create ?id label email name given_name password in
  match user with
  | Ok user -> Lwt.return user
  | Error msg -> raise (Exception (Pool_message.Error.show msg))
;;

let create_admin ?id label email name given_name password =
  let%lwt user = Repo.get_by_email label email in
  let%lwt () =
    match user with
    | Some _ ->
      Logs.err (fun m ->
        m
          "Can not create admin '%a' since the email is already taken"
          Entity.EmailAddress.pp
          email);
      raise (Exception "Email already taken")
    | None -> Lwt.return ()
  in
  let%lwt user =
    create ?id ~admin:true ~confirmed:true label email name given_name password
  in
  match user with
  | Ok user -> Lwt.return user
  | Error msg ->
    let msg = Pool_message.Error.show msg in
    Logs.err (fun m ->
      m "Can not create admin '%a': %s" Entity.EmailAddress.pp email msg);
    raise (Exception msg)
;;

let register_user ?id label email name given_name password password_confirmation
  =
  match
    Entity.Password.validate_password_confirmation
      password
      password_confirmation
  with
  | Error msg -> Lwt_result.fail @@ `Invalid_password_provided msg
  | Ok () ->
    let%lwt user = find_by_email_opt label email in
    (match user with
     | None ->
       create_user ?id label email name given_name password |> Lwt.map Result.ok
     | Some _ -> Lwt_result.fail `Already_registered)
;;

let login label email password =
  let%lwt user = find_by_email_opt label email in
  match user with
  | None -> Lwt_result.fail `Does_not_exist
  | Some user ->
    if Entity.HashedPassword.matches user.Entity.password password
    then Lwt_result.return user
    else Lwt_result.fail `Incorrect_password
;;

let start () = Lwt.return ()
let stop () = Lwt.return ()

let lifecycle =
  Sihl.Container.create_lifecycle
    "pool_user_service"
    ~dependencies:(fun () -> [ Pool_database.lifecycle ])
    ~start
    ~stop
;;

let register ?(commands = []) () =
  Repo.register_migration ();
  Repo.register_cleaner ();
  Sihl.Container.Service.create ~commands lifecycle
;;

module Web = struct
  let user_from_session = Web.user_from_session find_opt
  let user_from_token = Web.user_from_token find_opt
end
