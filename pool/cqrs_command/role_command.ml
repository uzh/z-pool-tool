module Create : sig
  type t = { name : string }

  val handle : t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { name : string }

  let handle = Utils.todo
  let can _ = Utils.todo
end
