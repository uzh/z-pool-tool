let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

let log_migration label =
  let tags = Database.Logger.Tags.create label in
  Logs.info (fun m -> m ~tags "Migrate database")
;;
