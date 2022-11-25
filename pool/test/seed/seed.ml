module Contacts = Seed_contacts

let create db_pool () =
  let%lwt () = Contacts.create db_pool in
  Lwt.return_unit
;;

let[@warning "-21"] cleanup db_pool () =
  failwith "Cleaning up";
  let%lwt () = Contacts.cleanup db_pool in
  Lwt.return_unit
;;
