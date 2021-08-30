(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)
module Migration = Sihl.Database.Migration.MariaDb
module User = Sihl_user.MariaDb
