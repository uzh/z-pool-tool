include Event

(* This can be cached in memory for a limited period of time *)
type translate = Settings.Language.t -> string -> string Lwt.t

let translate : translate = fun _ -> Utils.todo

type find_all = unit -> Entity.Property.t list Lwt.t

let find_all = Utils.todo
