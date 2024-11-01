include Entity

let run database_label contact_uuid =
  let%lwt fields = Custom_field.all_published database_label in
  let%lwt duplicates =
    Repo.find_similars database_label ~user_uuid:contact_uuid fields
  in
  let () =
    CCList.iter (fun duplicate -> print_endline (show duplicate)) duplicates
  in
  Lwt.return ()
;;

let find_by_contact database_label contact_uuid =
  let%lwt fields = Custom_field.all_published database_label in
  Repo.find_similars database_label ~user_uuid:contact_uuid fields
;;
