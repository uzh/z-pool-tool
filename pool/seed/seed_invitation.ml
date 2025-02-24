let invitations pool =
  let%lwt experiments, (_ : Query.t) = Experiment.find_all pool in
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
         let open CCList in
         let n = length filtered_contacts / 2 in
         let contacts = take n filtered_contacts in
         let open Invitation in
         let invitations =
           contacts
           >|= Invitation.create
           >|= fun invitation ->
           Created { invitations = [ invitation ]; mailing = None; experiment }
         in
         events @ invitations |> Lwt.return)
      []
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) events in
  Lwt.return_unit
;;
