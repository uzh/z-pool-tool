(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)
module Migration = Sihl.Database.Migration.MariaDb
module User = Sihl_user.MariaDb
module Token = Sihl_token.MariaDb
module PasswordReset = Sihl_user.Password_reset.MakeMariaDb (Token)
module EmailTemplate = Sihl_email.Template.MariaDb
module Queue = Sihl_queue.MariaDb
module BlockingEmail = Sihl_email.Smtp
module Email = Sihl_email.Queued (Queue) (BlockingEmail)
