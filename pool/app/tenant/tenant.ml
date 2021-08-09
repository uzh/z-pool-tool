include Entity
include Event

let find_by_participant = Sihl.todo
let find_by_user = Sihl.todo

type list_recruiters =
  { limit : int
  ; offset : int
  }

type handle_list_recruiters = list_recruiters -> Sihl.User.t list Lwt.t

type list_tenants =
  { limit : int
  ; offset : int
  ; fitler : string
  }

type handle_tenants = list_tenants -> t list Lwt.t
