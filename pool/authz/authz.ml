type predicate =
  | Can_edit_tenant
  | Can_add_recruiters

type object_ = string
type permission = predicate * object_

module Roles = struct
  let operator _ = Sihl.todo
end

type assign = Sihl.User.t -> permission -> unit Sihl.io

let assign : assign = fun _ _ -> Sihl.todo

type divest = Sihl.User.t -> permission -> unit Sihl.io

let divest : divest = fun _ _ -> Sihl.todo
