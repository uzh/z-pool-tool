module Admin = Command_admin
module Contact = Command_contact
module DefaultData = Command_default_data
module Mail = Command_mail
module Matcher = Command_matcher
module Migrate = Command_migrate
module Queue = Command_queue
module Seed = Command_seed
module SessionReminder = Command_session_reminder
module SystemEvent = Command_system_events
module Tenant_pool = Command_pool_tenant
module Utils = Command_utils
module UserImport = Command_import
module Worker = Command_worker

let version =
  Command_utils.make_no_args "version" "Show version of current executable." (fun () ->
    print_endline (Format.asprintf "Version: %s" Version.to_string);
    Lwt.return_some ())
;;
