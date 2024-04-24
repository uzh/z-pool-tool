type t =
  { user : Pool_user.t
  ; email_verified : Pool_user.EmailVerified.t option
  ; import_pending : Pool_user.ImportPending.t
  }
[@@deriving eq, show]

let user { user; _ } = user

let create ~email_verified (user : Pool_user.t) : t =
  { user
  ; email_verified
  ; import_pending = Pool_user.ImportPending.create false
  }
;;

let id ({ user; _ } : t) = user.Pool_user.id
let email_address ({ user; _ } : t) = user.Pool_user.email
let sexp_of_t t = t |> id |> Pool_user.Id.sexp_of_t
let full_name { user; _ } = Pool_user.user_fullname user
let full_name_reversed { user; _ } = Pool_user.user_lastname_firstname user
let filterable_by = None
let searchable_by = Pool_user.searchable_by
let sortable_by = Pool_user.sortable_by
let default_sort = Pool_user.default_sort
let default_query = Pool_user.default_query
