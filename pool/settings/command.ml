module ChangeSettings : sig
  type t

  val handle : t -> (Event.t list, string) Result.t
end = struct
  type t = Entity.setting list

  let[@warning "-27"] handle command = Sihl.todo
end
