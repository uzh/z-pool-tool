module Create : sig
  type t = { name : string }

  val handle : t -> (Role.event list, string) Result.t
  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t = { name : string }

  let handle = Sihl.todo
  let can _ _ = Sihl.todo
end
