module Contacts = Seed_contacts

let create db_pool () =
  let%lwt () = Contacts.create db_pool in
  Lwt.return_unit
;;

let cleanup _ () =
  let pool = Test_utils.Data.database_label in
  let%lwt () = Contacts.cleanup pool in
  Lwt.return_unit
;;
