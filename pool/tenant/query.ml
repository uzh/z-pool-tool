type list_recruiters =
  { limit : int
  ; offset : int
  }

type handle_list_recruiters = list_recruiters -> Sihl.User.t list Sihl.io

type list_tenants =
  { limit : int
  ; offset : int
  ; fitler : string
  }

type handle_tenants = list_tenants -> Entity.tenant list Sihl.io
