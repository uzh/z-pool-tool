let take_n n contacts =
  if CCList.length contacts > 10 then CCList.take n contacts else contacts
;;

let waiting_list pool =
  let%lwt experiments = Experiment.all pool in
  let%lwt waiting_list_events, invitation_events =
    let open Utils.Lwt_result.Infix in
    Lwt_list.fold_left_s
      (fun (waiting_lists, invitations) (experiment : Experiment.t) ->
         let open Experiment in
         let common_id = experiment |> id |> Id.to_common in
         match
           experiment |> direct_registration_disabled |> DirectRegistrationDisabled.value
         with
         | true ->
           let%lwt filtered_contacts =
             experiment
             |> filter
             |> Filter.(find_filtered_contacts pool (Matcher common_id))
             ||> CCResult.get_exn
           in
           let contacts = take_n 10 filtered_contacts in
           let waiting_lists =
             let experiment = Experiment.to_public experiment in
             (contacts
              |> CCList.map (fun contact ->
                Waiting_list.Created { Waiting_list.contact; experiment }))
             @ waiting_lists
           in
           let invitations =
             Invitation.Created { Invitation.contacts; mailing = None; experiment }
             :: invitations
           in
           (waiting_lists, invitations) |> Lwt.return
         | false -> Lwt.return (waiting_lists, invitations))
      ([], [])
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) invitation_events in
  let%lwt () = Lwt_list.iter_s (Waiting_list.handle_event pool) waiting_list_events in
  Lwt.return_unit
;;
