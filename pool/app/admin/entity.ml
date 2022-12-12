module Id = Pool_common.Id

type t = Sihl_user.t [@@deriving eq, show]

let sexp_of_t t = t.Sihl_user.id |> fun s -> Sexplib0.Sexp.Atom s
let user m = m
let create (user : Sihl_user.t) : t = user
let id ({ Sihl_user.id; _ } : t) = Id.of_string id
let email ({ Sihl_user.email; _ } : t) = email

module Duplicate = struct
  type t =
    { first : t
    ; second : t
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
