include Entity
include Event

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

type handle_tenants = list_tenants -> tenant list Lwt.t
