module Contacts = Seed_contacts
module Tags = Cleanup_tags

let create db_pool () =
  let%lwt () = Contacts.create db_pool in
  Lwt.return_unit
;;

let cleanup _ () =
  let pool = Test_utils.Data.database_label in
  let%lwt () = Contacts.cleanup pool in
  let%lwt () = Tags.cleanup pool in
  Lwt.return_unit
;;
