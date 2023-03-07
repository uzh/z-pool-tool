let take_n n contacts =
  if CCList.length contacts > 10 then CCList.take n contacts else contacts
;;

let waiting_list pool =
  let%lwt experiments, (_ : Query.t) = Experiment.find_all pool () in
  let to_public_experiment (experiment : Experiment.t) =
    Experiment.Public.
      { id = experiment.Experiment.id
      ; public_title = experiment.Experiment.public_title
      ; description = experiment.Experiment.description
      ; direct_registration_disabled =
          experiment.Experiment.direct_registration_disabled
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      }
  in
  let%lwt events =
    let open Utils.Lwt_result.Infix in
    Lwt_list.filter_map_s
      (fun (experiment : Experiment.t) ->
        let open Experiment in
        match
          experiment.direct_registration_disabled
          |> DirectRegistrationDisabled.value
        with
        | true ->
          let%lwt filtered_contacts =
            Filter.find_filtered_contacts
              pool
              (experiment.id |> Id.to_common)
              experiment.filter
            ||> CCResult.get_exn
          in
          let contacts = take_n 10 filtered_contacts in
          let experiment = to_public_experiment experiment in
          contacts
          |> CCList.map (fun contact ->
               Waiting_list.Created Waiting_list.{ contact; experiment })
          |> CCOption.pure
          |> Lwt.return
        | false -> Lwt.return None)
      experiments
    ||> CCList.flatten
  in
  let%lwt () = Lwt_list.iter_s (Waiting_list.handle_event pool) events in
  Lwt.return_unit
;;
