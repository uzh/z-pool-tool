let user_from_token find_user ?(key = "user_id") db_ctx read_token req
  : Entity.t option Lwt.t
  =
  match Sihl.Web.Request.bearer_token req with
  | Some token ->
    let%lwt user_id = read_token db_ctx token ~k:key in
    (match user_id with
     | None -> Lwt.return None
     | Some user_id -> find_user db_ctx user_id)
  | None -> Lwt.return None
;;

let user_from_session find_user ?cookie_key ?secret ?(key = "user_id") db_ctx req
  : Entity.t option Lwt.t
  =
  match Sihl.Web.Session.find ?cookie_key ?secret key req with
  | Some user_id -> find_user db_ctx user_id
  | None -> Lwt.return None
;;

let user_from_token ?key db_ctx = user_from_token ?key Repo.find_opt db_ctx
let user_from_session ?cookie_key ?secret ?key db_ctx = user_from_session ?cookie_key ?secret ?key Repo.find_opt db_ctx
