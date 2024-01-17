module Id = Pool_common.Id

type t =
  { user : Sihl_user.t
  ; email_verified : Pool_user.EmailVerified.t option
  ; import_pending : Pool_user.ImportPending.t
  }
[@@deriving eq, show]

let user { user; _ } = user

let create ~email_verified (user : Sihl_user.t) : t =
  { user
  ; email_verified
  ; import_pending = Pool_user.ImportPending.create false
  }
;;

let id ({ user; _ } : t) = Id.of_string user.Sihl_user.id

let email_address ({ user; _ } : t) =
  user.Sihl_user.email |> Pool_user.EmailAddress.of_string
;;

let sexp_of_t t = t |> id |> Id.value |> fun s -> Sexplib0.Sexp.Atom s

let full_name { user; _ } =
  Sihl_user.[ user.given_name; user.name ]
  |> CCList.filter_map CCFun.id
  |> CCString.concat " "
;;

let full_name_reversed { user; _ } =
  Sihl_user.[ user.name; user.given_name ]
  |> CCList.filter_map CCFun.id
  |> CCString.concat " "
;;

let filterable_by = None
let searchable_by = Pool_user.searchable_by
let sortable_by = Pool_user.sortable_by
let default_sort = Pool_user.default_sort
let default_query = Pool_user.default_query

module Duplicate = struct
  type t =
    { first : t
    ; second : t
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
