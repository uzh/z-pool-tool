include Entity
include Event

(* This can be cached in memory for a limited period of time *)

(* type translate = Settings.Language.t -> string -> string Lwt.t *)

let translate = Utils.todo
let find = Repo.find
let find_all = Repo.find_all
