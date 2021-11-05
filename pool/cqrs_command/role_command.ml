module Create : sig
  type t = { name : string }

  val handle : t -> (Pool_event.t list, Pool_common.Error.t) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { name : string }

  let handle = Utils.todo
  let can _ = Utils.todo
end
