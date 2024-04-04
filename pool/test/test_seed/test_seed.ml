module Contacts = Test_seed_contacts

let create db_pool () =
  let%lwt () = Contacts.create db_pool in
  Lwt.return_unit
;;

let cleanup _ () = Database.clean_all_exn Test_utils.Data.database_label
