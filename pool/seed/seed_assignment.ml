let assignment pool =
  let open Utils.Lwt_result.Infix in
  let%lwt offline_experiments, online_experiments =
    Experiment.all pool
    ||> CCList.partition (fun { Experiment.online_experiment; _ } ->
      online_experiment |> CCOption.is_none)
  in
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
      offline_experiments
  in
  let%lwt time_window_invitations =
    Lwt_list.map_s
      (fun experiment ->
         let%lwt experiment_invitations, (_ : Query.t) =
           Invitation.find_by_experiment pool experiment.Experiment.id
         in
         let%lwt time_window =
           Time_window.query_by_experiment pool experiment.Experiment.id
           ||> fun (time_windows, _) -> time_windows |> CCList.hd
         in
         Lwt.return (time_window, experiment_invitations))
      online_experiments
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
    @ CCList.flat_map
        (fun (time_window, invitations) ->
           invitations
           |> CCList.take (CCList.length invitations / 2)
           |> CCList.flat_map (fun ({ Invitation.contact; _ } : Invitation.t) ->
             let assign { Time_window.id; _ } =
               Assignment.(Created (create contact, id))
             in
             [ assign time_window ]))
        time_window_invitations
  in
  let%lwt () = Lwt_list.iter_s (Assignment.handle_event pool) events in
  Lwt.return_unit
;;
