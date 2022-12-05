let assignment pool =
  let open Utils.Lwt_result.Infix in
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt session_invitations =
    Lwt_list.map_s
      (fun experiment ->
        let%lwt experiment_invitations =
          Invitation.find_by_experiment pool experiment.Experiment.id
          ||> CCResult.get_exn
        in
        let%lwt session =
          Session.find_all_for_experiment pool experiment.Experiment.id
          >|+ CCList.hd
          ||> CCResult.get_exn
        in
        Lwt.return (session, experiment_invitations))
      experiments
  in
  let events =
    CCList.flat_map
      (fun (session, invitations) ->
        let invitations =
          CCList.take (CCList.length invitations / 2) invitations
        in
        CCList.flat_map
          (fun (invitation : Invitation.t) ->
            [ Assignment.Created
                Assignment.
                  { contact = invitation.Invitation.contact
                  ; session_id = session.Session.id
                  }
            ])
          invitations)
      session_invitations
  in
  let%lwt () = Lwt_list.iter_s (Assignment.handle_event pool) events in
  Lwt.return_unit
;;
