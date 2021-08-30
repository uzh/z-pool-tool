module Update : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (Settings.event list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Utils.todo
  let can = Utils.todo
end
