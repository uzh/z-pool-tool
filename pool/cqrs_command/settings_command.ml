module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (Pool_event.t list, Pool_common.Message.t) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Utils.todo
  let can = Utils.todo
end
