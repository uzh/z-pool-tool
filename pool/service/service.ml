(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)
let log_src = Logs.Src.create "email"

module Logs = (val Logs.src_log log_src : Logs.LOG)
module Migration = Sihl.Database.Migration.MariaDb

module User = struct
  include Sihl_user.MariaDb
  include Sihl_user

  let sexp_of_t t = t.id |> fun s -> Sexplib0.Sexp.Atom s
end

module Token = Sihl_token.MariaDb
module PasswordReset = Sihl_user.Password_reset.MakeMariaDb (Token)
module Storage = Sihl_storage.MariaDb
