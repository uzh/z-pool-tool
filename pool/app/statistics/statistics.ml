include Entity

let create database_label ?(period = default_period) () =
  let open Repo in
  let%lwt active_contacts = active_contacts database_label in
  let%lwt pending_contact_imports = pending_contact_imports database_label in
  let%lwt assignments_created = assignments_created database_label period in
  let%lwt invitations_sent = invitations_sent database_label period in
  let%lwt sign_up_count = sign_up_count database_label period in
  Lwt.return
    { active_contacts
    ; pending_contact_imports
    ; assignments_created
    ; invitations_sent
    ; sign_up_count
    }
;;
