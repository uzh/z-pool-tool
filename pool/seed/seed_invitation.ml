let invitations pool =
  let%lwt experiments = Experiment.all pool in
  let%lwt events =
    let open Utils.Lwt_result.Infix in
    Lwt_list.fold_left_s
      (fun events experiment ->
         let%lwt filtered_contacts =
           Filter.(
             find_filtered_contacts
               pool
               (Matcher (experiment.Experiment.id |> Experiment.Id.to_common))
               experiment.Experiment.filter)
           ||> CCResult.get_exn
         in
         let n = CCList.length filtered_contacts / 2 in
         let contacts = CCList.take n filtered_contacts in
         Lwt.return
           (events
            @ CCList.map
                (fun contact ->
                   Invitation.Created
                     { Invitation.contacts = contact |> CCList.pure
                     ; mailing = None
                     ; experiment
                     })
                contacts))
      []
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) events in
  Lwt.return_unit
;;
