let user_from_token find_user ?(key = "user_id") label read_token req
  : Entity.t option Lwt.t
  =
  match Sihl.Web.Request.bearer_token req with
  | Some token ->
    let%lwt user_id = read_token label token ~k:key in
    (match user_id with
     | None -> Lwt.return None
     | Some user_id -> find_user label user_id)
  | None -> Lwt.return None
;;

let user_from_session find_user ?cookie_key ?secret ?(key = "user_id") label req
  : Entity.t option Lwt.t
  =
  match Sihl.Web.Session.find ?cookie_key ?secret key req with
  | Some user_id -> find_user label user_id
  | None -> Lwt.return None
;;

let user_from_token = user_from_token Repo.find_opt
let user_from_session = user_from_session Repo.find_opt
