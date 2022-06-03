let waiting_list pool =
  let%lwt experiments = Experiment.find_all pool () in
  let to_public_experiment (experiment : Experiment.t) =
    Experiment.Public.
      { id = experiment.Experiment.id
      ; description = experiment.Experiment.description
      ; waiting_list_disabled = experiment.Experiment.waiting_list_disabled
      ; direct_registration_disabled =
          experiment.Experiment.direct_registration_disabled
      }
  in
  let%lwt events =
    Lwt_list.map_s
      (fun experiment ->
        let%lwt filtered_contacts =
          Contact.find_filtered pool experiment.Experiment.filter ()
        in
        let n = Random.int (CCList.length filtered_contacts) in
        let contact = CCList.nth filtered_contacts n in
        let experiment = to_public_experiment experiment in
        let waiting_list = Waiting_list.{ contact; experiment } in
        Waiting_list.Created waiting_list |> Lwt.return)
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Waiting_list.handle_event pool) events in
  Lwt.return_unit
;;
