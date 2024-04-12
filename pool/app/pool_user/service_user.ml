include Sihl.Contract.User

let log_src = Logs.Src.create "pool.user"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let find_opt = Repo.get

let find label user_id =
  let%lwt m_user = find_opt label user_id in
  match m_user with
  | Some user -> Lwt.return user
  | None ->
    Logs.err (fun m -> m "User not found with id %s" user_id);
    raise (Sihl.Contract.User.Exception "User not found")
;;

let find_by_email_opt = Repo.get_by_email

let find_by_email label email =
  let%lwt user = find_by_email_opt label email in
  match user with
  | Some user -> Lwt.return user
  | None ->
    Logs.err (fun m -> m "User not found with email %s" email);
    raise (Sihl.Contract.User.Exception "User not found")
;;

let update_details ~user:_ ~email:_ ~username:_ = failwith "update()"

let update ?email ?username ?name ?given_name ?status label user =
  let updated =
    { user with
      email = Option.value ~default:user.email email
    ; username =
        (match username with
         | Some username -> Some username
         | None -> user.username)
    ; name =
        (match name with
         | Some name -> Some name
         | None -> user.name)
    ; given_name =
        (match given_name with
         | Some given_name -> Some given_name
         | None -> user.given_name)
    ; status = Option.value ~default:user.status status
    }
  in
  let%lwt () = Repo.update label updated in
  find label user.id
;;

let update_password
  ?(password_policy = Sihl.Contract.User.default_password_policy)
  label
  user
  ~old_password
  ~new_password
  ~new_password_confirmation
  =
  match
    validate_change_password
      user
      ~old_password
      ~new_password
      ~new_password_confirmation
      ~password_policy
  with
  | Ok () ->
    let updated_user =
      match set_user_password user new_password with
      | Ok user -> user
      | Error msg ->
        Logs.err (fun m ->
          m "Can not update password of user '%s': %s" user.email msg);
        raise (Sihl.Contract.User.Exception msg)
    in
    let%lwt () = Repo.update label updated_user in
    find label user.id |> Lwt.map Result.ok
  | Error msg -> Lwt.return @@ Error msg
;;

let set_password
  ?(password_policy = default_password_policy)
  label
  user
  ~password
  ~password_confirmation
  =
  let%lwt result =
    validate_new_password ~password ~password_confirmation ~password_policy
    |> Lwt.return
  in
  match result with
  | Error msg -> Lwt.return @@ Error msg
  | Ok () ->
    let%lwt result = Repo.get label user.id in
    (* Re-fetch user to make sure that we have an up-to-date model *)
    let%lwt user =
      match result with
      | Some user -> Lwt.return user
      | None -> raise (Sihl.Contract.User.Exception "Failed to create user")
    in
    let updated_user =
      match set_user_password user password with
      | Ok user -> user
      | Error msg ->
        Logs.err (fun m ->
          m "Can not set password of user %s: %s" user.email msg);
        raise (Sihl.Contract.User.Exception msg)
    in
    let%lwt () = Repo.update label updated_user in
    find label user.id |> Lwt.map Result.ok
;;

let create
  ?id
  label
  ~email
  ~password
  ~username
  ~name
  ~given_name
  ~admin
  confirmed
  =
  let user =
    make ?id ~email ~password ~username ~name ~given_name ~admin confirmed
  in
  match user with
  | Ok user ->
    let%lwt () = Repo.insert label user in
    let%lwt user = find label user.id in
    Lwt.return (Ok user)
  | Error msg -> raise (Sihl.Contract.User.Exception msg)
;;

let create_user ?id ?username ?name ?given_name label ~password email =
  let%lwt user =
    create
      ?id
      label
      ~password
      ~username
      ~name
      ~given_name
      ~admin:false
      ~email
      false
  in
  match user with
  | Ok user -> Lwt.return user
  | Error msg -> raise (Sihl.Contract.User.Exception msg)
;;

let create_admin ?id ?username ?name ?given_name label ~password email =
  let%lwt user = Repo.get_by_email label email in
  let%lwt () =
    match user with
    | Some _ ->
      Logs.err (fun m ->
        m "Can not create admin '%s' since the email is already taken" email);
      raise (Sihl.Contract.User.Exception "Email already taken")
    | None -> Lwt.return ()
  in
  let%lwt user =
    create
      ?id
      label
      ~password
      ~username
      ~name
      ~given_name
      ~admin:true
      ~email
      true
  in
  match user with
  | Ok user -> Lwt.return user
  | Error msg ->
    Logs.err (fun m -> m "Can not create admin '%s': %s" email msg);
    raise (Sihl.Contract.User.Exception msg)
;;

let register_user
  ?id
  ?(password_policy = default_password_policy)
  ?username
  ?name
  ?given_name
  label
  email
  ~password
  ~password_confirmation
  =
  match
    validate_new_password ~password ~password_confirmation ~password_policy
  with
  | Error msg -> Lwt_result.fail @@ `Invalid_password_provided msg
  | Ok () ->
    let%lwt user = find_by_email_opt label email in
    (match user with
     | None ->
       create_user label ?id ?username ?name ?given_name ~password email
       |> Lwt.map Result.ok
     | Some _ -> Lwt_result.fail `Already_registered)
;;

let login label email ~password =
  let open Sihl.Contract.User in
  let%lwt user = find_by_email_opt label email in
  match user with
  | None -> Lwt_result.fail `Does_not_exist
  | Some user ->
    if matches_password password user
    then Lwt_result.return user
    else Lwt_result.fail `Incorrect_password
;;

let start () = Lwt.return ()
let stop () = Lwt.return ()

let lifecycle =
  Sihl.Container.create_lifecycle
    Sihl.Contract.User.name
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
