type config = { token_length : int option }

let config token_length = { token_length }

let schema =
  let open Conformist in
  make Field.[ Conformist.optional (int ~default:80 "TOKEN_LENGTH") ] config
;;

let is_valid_token token =
  let open Repo.Model in
  Status.equal token.status Status.Active
  && Ptime.is_later token.expires_at ~than:(Ptime_clock.now ())
;;

let make id ?(expires_in = Sihl.Time.OneDay) ?now ?(length = 80) data =
  let open Repo.Model in
  let value = Sihl.Random.base64 length in
  let expires_in = Sihl.Time.duration_to_span expires_in in
  let now = CCOption.value ~default:(Ptime_clock.now ()) now in
  let expires_at =
    match Ptime.add_span now expires_in with
    | Some expires_at -> expires_at
    | None -> failwith ("Could not parse expiry date for token with id " ^ id)
  in
  let status = Status.Active in
  let created_at = Ptime_clock.now () in
  { id; value; data; status; expires_at; created_at }
;;

let create ?secret:_ ?expires_in label data =
  let open Repo.Model in
  let id = Pool_common.Id.(create () |> value) in
  let length = CCOption.value ~default:30 (Sihl.Configuration.read schema).token_length in
  let token = make id ?expires_in ~length data in
  let%lwt () = Repo.insert label token in
  Repo.find_by_id label id |> Lwt.map (fun token -> token.value)
;;

let read ?secret:_ ?force label token_value ~k =
  let open Repo.Model in
  let%lwt token = Repo.find_opt label token_value in
  match token with
  | None -> Lwt.return None
  | Some token ->
    (match is_valid_token token, force with
     | true, _ | false, Some () ->
       (match CCList.find_opt (fun (key, _) -> CCString.equal k key) token.data with
        | Some (_, value) -> Lwt.return (Some value)
        | None -> Lwt.return None)
     | false, None -> Lwt.return None)
;;

let read_all ?secret:_ ?force label token =
  let open Repo.Model in
  let%lwt token = Repo.find label token in
  match is_valid_token token, force with
  | true, _ | false, Some () -> Lwt.return (Some token.data)
  | false, None -> Lwt.return None
;;

let verify ?secret:_ label token =
  let%lwt token = Repo.find_opt label token in
  match token with
  | Some _ -> Lwt.return true
  | None -> Lwt.return false
;;

let deactivate label token =
  let open Repo.Model in
  let%lwt token = Repo.find label token in
  let updated = { token with status = Status.Inactive } in
  Repo.update label updated
;;

let activate label token =
  let open Repo.Model in
  let%lwt token = Repo.find label token in
  let updated = { token with status = Status.Active } in
  Repo.update label updated
;;

let is_active label token =
  let open Repo.Model in
  let%lwt token = Repo.find label token in
  match token.status with
  | Status.Active -> Lwt.return true
  | Status.Inactive -> Lwt.return false
;;

let is_expired ?secret:_ label token =
  let open Repo.Model in
  let%lwt token = Repo.find label token in
  Lwt.return (Ptime.is_earlier token.expires_at ~than:(Ptime_clock.now ()))
;;

let is_valid ?secret:_ label token =
  let open Repo.Model in
  let%lwt token = Repo.find_opt label token in
  match token with
  | None -> Lwt.return false
  | Some token ->
    (match token.status with
     | Status.Inactive -> Lwt.return false
     | Status.Active ->
       Lwt.return (Ptime.is_later token.expires_at ~than:(Ptime_clock.now ())))
;;

let start () =
  (* Make sure that configuration is valid *)
  Sihl.Configuration.require schema;
  Lwt.return ()
;;

let stop () = Lwt.return ()

let lifecycle =
  Sihl.Container.create_lifecycle
    "pool_token"
    ~dependencies:(fun () -> Repo.lifecycles)
    ~start
    ~stop
;;

let register () =
  Repo.register_migration ();
  Repo.register_cleaner ();
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
