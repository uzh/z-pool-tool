type permission = string * string option
type assign = Sihl.User.t -> permission list -> unit Lwt.t

let assign : assign = fun user permissions -> Repo.insert user.id permissions

type divest = Sihl.User.t -> permission list -> unit Lwt.t

let divest : divest = fun user permissions -> Repo.delete user.id permissions

type can = Sihl.User.t -> permission -> bool Lwt.t

let can : can = fun user permission -> Repo.has user.id permission
