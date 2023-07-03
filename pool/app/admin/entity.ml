module Id = Pool_common.Id

type t = { user : Sihl_user.t } [@@deriving eq, show]

let user { user } = user
let create (user : Sihl_user.t) : t = { user }
let id ({ user; _ } : t) = Id.of_string user.Sihl_user.id
let email ({ user; _ } : t) = user.Sihl_user.email
let sexp_of_t t = t |> id |> Id.value |> fun s -> Sexplib0.Sexp.Atom s

let full_name { user } =
  Sihl_user.[ user.given_name; user.name ]
  |> CCList.filter_map CCFun.id
  |> CCString.concat " "
;;

module Duplicate = struct
  type t =
    { first : t
    ; second : t
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
