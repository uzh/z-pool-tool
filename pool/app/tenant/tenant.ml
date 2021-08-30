include Entity
include Event

let find_by_participant = Utils.todo
let find_by_user = Utils.todo

type list_recruiters =
  { limit : int
  ; offset : int
  }

type handle_list_recruiters = list_recruiters -> Sihl_user.t list Lwt.t

type list_tenants =
  { limit : int
  ; offset : int
  ; fitler : string
  }

type handle_tenants = list_tenants -> t list Lwt.t
