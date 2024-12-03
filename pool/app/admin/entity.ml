open CCFun.Infix

module Id = struct
  include Pool_model.Base.Id

  let of_common = Pool_common.Id.value %> of_string
  let to_common = value %> Pool_common.Id.of_string
  let of_user = Pool_user.Id.value %> of_string
  let to_user = value %> Pool_user.Id.of_string
end

type t =
  { user : Pool_user.t
  ; email_verified : Pool_user.EmailVerified.t option
  ; import_pending : Pool_user.ImportPending.t
  }
[@@deriving eq, show]

let user { user; _ } = user

let create ~email_verified (user : Pool_user.t) : t =
  { user; email_verified; import_pending = Pool_user.ImportPending.create false }
;;

let id ({ user; _ } : t) = user.Pool_user.id |> Id.of_user
let email_address ({ user; _ } : t) = user.Pool_user.email
let sexp_of_t t = t |> id |> Id.sexp_of_t
let fullname { user; _ } = Pool_user.fullname user
let fullname_reversed { user; _ } = Pool_user.fullname ~reversed:true user
let filterable_by = None
let searchable_by = Pool_user.searchable_by
let sortable_by = Pool_user.sortable_by
let default_sort = Pool_user.default_sort
let default_query = Pool_user.default_query
