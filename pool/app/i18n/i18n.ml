type event = Event.t

(* This can be cached in memory for a limited period of time *)
type translate = Settings.language -> string -> string Lwt.t
