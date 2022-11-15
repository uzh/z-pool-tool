let invitations pool =
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt events =
    Lwt_list.fold_left_s
      (fun events experiment ->
        let%lwt filtered_contacts =
          Contact.find_filtered
            pool
            experiment.Experiment.id
            (experiment |> Experiment.filter_predicate)
          |> Lwt.map CCResult.get_exn
        in
        let n = CCList.length filtered_contacts / 2 in
        let contacts = CCList.take n filtered_contacts in
        Lwt.return
          (events
          @ CCList.map
              (fun contact ->
                Invitation.Created (contact |> CCList.pure, experiment))
              contacts))
      []
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) events in
  Lwt.return_unit
;;
