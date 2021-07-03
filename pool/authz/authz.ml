type permission = string * string
type assign = Sihl.User.t -> permission list -> unit Sihl.io

let assign : assign = fun _ _ -> Sihl.todo

type divest = Sihl.User.t -> permission list -> unit Sihl.io

let divest : divest = fun _ _ -> Sihl.todo

type can = Sihl.User.t -> permission -> bool Sihl.io

let can _ _ = Sihl.todo
