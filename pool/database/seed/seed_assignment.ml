let assignment pool =
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt session_invitations =
    Lwt_list.fold_left_s
      (fun invitations experiment ->
        let%lwt experiment_invitations =
          Invitation.find_by_experiment pool experiment.Experiment.id
          |> Lwt.map CCResult.get_exn
        in
        let%lwt session =
          Session.find_all_for_experiment pool experiment.Experiment.id
          |> Lwt.map (fun s -> Seed_utils.get_random (s |> CCResult.get_exn))
        in
        Lwt.return ((session, experiment_invitations) :: invitations))
      []
      experiments
  in
  let events =
    CCList.fold_left
      (fun (events : Assignment.event list) (session, invitations) ->
        events
        @ CCList.flat_map
            (fun (invitation : Invitation.t) ->
              let random = Random.bool () in
              match random with
              | false -> []
              | true ->
                [ Assignment.Created
                    Assignment.
                      { contact = invitation.Invitation.contact
                      ; session_id = session.Session.id
                      }
                ])
            invitations)
      []
      session_invitations
  in
  let%lwt () = Lwt_list.iter_s (Assignment.handle_event pool) events in
  Lwt.return_unit
;;
