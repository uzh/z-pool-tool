type list_recruiters =
  { limit : int
  ; offset : int
  }

type handle_list_recruiters = list_recruiters -> Sihl.User.t list Sihl.io

type list_clients =
  { limit : int
  ; offset : int
  ; fitler : string
  }

type handle_clients = list_clients -> Entity.client list Sihl.io
