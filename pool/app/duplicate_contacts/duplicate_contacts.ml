include Entity

let make_custom_field_columns pool = Custom_field.all_published pool

let run database_label contact_uuid =
  let%lwt fields = make_custom_field_columns database_label in
  let%lwt duplicates =
    Repo.find_similars database_label ~user_uuid:contact_uuid fields
  in
  let () =
    CCList.iter (fun duplicate -> print_endline (show duplicate)) duplicates
  in
  Lwt.return ()
;;
