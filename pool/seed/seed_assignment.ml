let assignment pool =
  let open Utils.Lwt_result.Infix in
  let%lwt experiments = Experiment.all pool in
  let%lwt session_invitations =
    Lwt_list.map_s
      (fun experiment ->
         let%lwt experiment_invitations, (_ : Query.t) =
           Invitation.find_by_experiment pool experiment.Experiment.id
         in
         let%lwt session =
           Session.find_all_for_experiment pool experiment.Experiment.id ||> CCList.hd
         in
         let%lwt follow_ups = Session.find_follow_ups pool session.Session.id in
         Lwt.return (session, follow_ups, experiment_invitations))
      experiments
  in
  let events =
    CCList.flat_map
      (fun (session, follow_ups, invitations) ->
         invitations
         |> CCList.take (CCList.length invitations / 2)
         |> CCList.flat_map (fun ({ Invitation.contact; _ } : Invitation.t) ->
           let assign { Session.id; _ } = Assignment.(Created (create contact, id)) in
           assign session :: CCList.map assign follow_ups))
      session_invitations
  in
  let%lwt () = Lwt_list.iter_s (Assignment.handle_event pool) events in
  Lwt.return_unit
;;
