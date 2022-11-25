module Contacts = Seed_contacts

let create db_pool () =
  let%lwt () = Contacts.create db_pool in
  Lwt.return_unit
;;
