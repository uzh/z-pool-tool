include Guardian.Make (Role.Actor) (Role.Role) (Role.Target)

module Uuid = struct
  include Uuid

  let actor_of fcn = CCFun.(fcn %> Actor.of_string_exn)
  let target_of fcn = CCFun.(fcn %> Target.of_string_exn)
end
