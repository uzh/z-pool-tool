let get_or_failwith = Pool_common.Utils.get_or_failwith

let invitations pool () =
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt events =
    Lwt_list.map_s
      (fun experiment ->
        let%lwt filtered_participants =
          Subject.find_filtered pool experiment.Experiment.filter ()
        in
        let n = Random.int (List.length filtered_participants) in
        let subject = CCList.nth filtered_participants n in
        let invitation = Invitation.{ subject; experiment } in
        Invitation.Created invitation |> Lwt.return)
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Invitation.handle_event pool) events in
  Lwt.return_unit
;;
