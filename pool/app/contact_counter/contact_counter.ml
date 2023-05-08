let update_on_invitation_sent m =
  Contact.
    { m with
      num_invitations = m.num_invitations |> NumberOfInvitations.increment
    }
;;

let update_on_session_signup m sessions =
  Contact.
    { m with
      num_assignments =
        NumberOfAssignments.increment m.num_assignments (CCList.length sessions)
    }
;;

let update_on_assignment_from_waiting_list = update_on_session_signup

let update_on_session_closing
  contact
  no_show
  participated
  increment_num_participaton
  =
  let open Contact in
  let open Assignment in
  match NoShow.value no_show, Participated.value participated with
  | true, true ->
    Error Pool_common.Message.(MutuallyExclusive Field.(Participated, NoShow))
  | true, false -> Ok (increment_num_no_shows contact)
  | false, true | false, false ->
    contact
    |> increment_num_show_ups
    |> fun contact ->
    (if IncrementParticipationCount.value increment_num_participaton
     then increment_num_participations contact
     else contact)
    |> CCResult.return
;;

let update_on_session_cancellation assignments m =
  Contact.
    { m with
      num_assignments =
        NumberOfAssignments.decrement
          m.num_assignments
          (CCList.length assignments)
    }
;;

let update_on_assignment_cancellation = update_on_session_cancellation

let update_on_assignment_deletion
  assignments
  contact
  decrement_participation_count
  =
  CCList.fold_left
    (fun (contact, closed_assignment)
         { Assignment.no_show; participated; canceled_at; _ } ->
      let open Contact in
      let open Assignment in
      let closed_assignment =
        closed_assignment
        || CCOption.is_some no_show
        || CCOption.is_some participated
      in
      let contact =
        match no_show with
        | None -> contact
        | Some no_show ->
          (match NoShow.value no_show with
           | true -> decrement_num_no_shows contact
           | false -> decrement_num_show_ups contact)
      in
      match canceled_at with
      | None -> decrement_num_assignments contact, closed_assignment
      | Some _ -> contact, closed_assignment)
    (contact, false)
    assignments
  |> fun (contact, closed_assignment) ->
  if Assignment.IncrementParticipationCount.value decrement_participation_count
     && closed_assignment
  then contact |> Contact.decrement_num_participations
  else contact
;;
