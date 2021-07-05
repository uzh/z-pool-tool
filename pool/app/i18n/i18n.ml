include Event

(* This can be cached in memory for a limited period of time *)
type translate = Settings.language -> string -> string Lwt.t

let translate : translate = fun _ _ -> Sihl.todo

type find_all = unit -> Entity.property list Lwt.t

let find_all _ = Sihl.todo
