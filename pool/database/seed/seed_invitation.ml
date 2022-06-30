let invitations pool =
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt events =
    Lwt_list.map_s
      (fun experiment ->
        let%lwt filtered_contacts =
          Contact.find_filtered
            pool
            experiment.Experiment.id
            experiment.Experiment.filter
        in
        let n = Random.int (CCList.length filtered_contacts) in
        let contact = CCList.nth filtered_contacts n in
        let invitation = Invitation.{ contact; experiment } in
        Invitation.Created invitation |> Lwt.return)
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) events in
  Lwt.return_unit
;;
