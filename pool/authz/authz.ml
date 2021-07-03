type permission = string * string
type assign = Sihl.User.t -> permission list -> unit Lwt.t

let assign : assign = fun _ _ -> Sihl.todo

type divest = Sihl.User.t -> permission list -> unit Lwt.t

let divest : divest = fun _ _ -> Sihl.todo

type can = Sihl.User.t -> permission -> bool Lwt.t

let can _ _ = Sihl.todo
