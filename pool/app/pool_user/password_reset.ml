let log_src = Logs.Src.create "password.reset"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let create_reset_token label email =
  let%lwt user = Service_user.find_by_email_opt label email in
  match user with
  | Some { Entity.id; _ } ->
    Pool_token.create
      label
      ~expires_in:Sihl.Time.OneDay
      [ "user_id", Entity.Id.value id ]
    |> Lwt.map CCOption.some
  | None ->
    let tags = Database.Logger.Tags.create label in
    Logs.warn (fun m ->
      m ~tags "No user found with email %a" Entity.EmailAddress.pp email);
    Lwt.return None
;;

let reset_password ~token label password password_confirmation =
  let%lwt user_id = Pool_token.read label token ~k:"user_id" in
  match user_id with
  | None -> Lwt.return_error Pool_message.(Error.Invalid Field.Token)
  | Some user_id ->
    let open Utils.Lwt_result.Infix in
    let%lwt user = Service_user.find label (Entity.Id.of_string user_id) in
    let* (_ : Entity.t) =
      Service_user.set_password label user password password_confirmation
    in
    Lwt.return_ok ()
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    Sihl.Contract.Password_reset.name
    ~dependencies:(fun () -> [ Pool_token.lifecycle; Service_user.lifecycle ])
;;

let register () = Sihl.Container.Service.create lifecycle
