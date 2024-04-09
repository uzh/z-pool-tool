(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)

module User = struct
  include Sihl_user.MariaDb
  include Sihl_user

  let sexp_of_t t = t.id |> fun s -> Sexplib0.Sexp.Atom s
end

module Storage = Sihl_storage.MariaDb
