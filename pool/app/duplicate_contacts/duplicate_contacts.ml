include Entity

let run database_label contact_uuid =
  let%lwt duplicates =
    Repo.find_similars database_label ~user_uuid:contact_uuid
  in
  let () =
    CCList.iter (fun duplicate -> print_endline (show duplicate)) duplicates
  in
  Lwt.return ()
;;
