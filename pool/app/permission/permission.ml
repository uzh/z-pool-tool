include Core

type assign = Sihl_user.t -> Core.permission list -> unit Lwt.t

let assign : assign =
 fun user permissions -> Repo.insert user.Sihl_user.id permissions
;;

type divest = Sihl_user.t -> Core.permission list -> unit Lwt.t

let divest : divest =
 fun user permissions -> Repo.delete user.Sihl_user.id permissions
;;

type can = Sihl_user.t -> any_of:Core.permission list -> bool Lwt.t

let can : can =
 fun user ~any_of:any_of_these ->
  let%lwt permissions = Repo.find_all_by_user user.Sihl_user.id in
  let intersection =
    (* TODO to improve performance, abort if one match was found *)
    CCList.inter ~eq:Core.equal_permission permissions any_of_these
  in
  Lwt.return @@ (List.length intersection > 0)
;;
