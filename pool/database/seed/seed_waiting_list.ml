let waiting_list pool =
  let%lwt experiments = Experiment.find_all pool () in
  let to_public_experiment (experiment : Experiment.t) =
    Experiment_type.
      { id = experiment.Experiment.id
      ; description = experiment.Experiment.description
      ; sessions = []
      }
  in
  let%lwt events =
    Lwt_list.map_s
      (fun experiment ->
        let%lwt filtered_subjects =
          Subject.find_filtered pool experiment.Experiment.filter ()
        in
        let n = Random.int (CCList.length filtered_subjects) in
        let subject = CCList.nth filtered_subjects n in
        let experiment = to_public_experiment experiment in
        let waiting_list = Waiting_list.{ subject; experiment } in
        Waiting_list.Created waiting_list |> Lwt.return)
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Waiting_list.handle_event pool) events in
  Lwt.return_unit
;;
