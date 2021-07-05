module Change : sig
  type t = { new_settings : (string * string) list }

  val handle : t -> (I18n.event list, string) Result.t
  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t = { new_settings : (string * string) list }

  let handle = Sihl.todo
  let can = Sihl.todo
end
