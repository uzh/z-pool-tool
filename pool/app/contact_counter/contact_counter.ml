let update_on_invitation_sent = Contact.update_num_invitations ~step:1

let update_on_session_signup m sessions =
  Contact.update_num_assignments ~step:(CCList.length sessions) m
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
  | true, false -> Ok (update_num_no_shows ~step:1 contact)
  | false, (true | false) ->
    contact
    |> update_num_show_ups ~step:1
    |> fun contact ->
    (if IncrementParticipationCount.value increment_num_participaton
     then update_num_participations ~step:1 contact
     else contact)
    |> CCResult.return
;;

let update_on_session_cancellation assignments m =
  Contact.update_num_assignments
    ~step:(assignments |> CCList.length |> CCInt.neg)
    m
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
          if NoShow.value no_show
          then update_num_no_shows ~step:(-1) contact
          else update_num_show_ups ~step:(-1) contact
      in
      match canceled_at with
      | None -> update_num_assignments ~step:(-1) contact, closed_assignment
      | Some _ -> contact, closed_assignment)
    (contact, false)
    assignments
  |> fun (contact, closed_assignment) ->
  if Assignment.IncrementParticipationCount.value decrement_participation_count
     && closed_assignment
  then Contact.update_num_participations ~step:(-1) contact
  else contact
;;

(* TODO: Should this be skipped as long the session is not closed? *)
let update_on_assignment_update
  { Assignment.no_show; participated; contact; _ }
  updated_no_show
  updated_participated
  participated_in_other_assignments
  =
  let open Assignment in
  let open Contact in
  let open CCOption.Infix in
  let contact =
    let open NoShow in
    (* update_num_show_ups is missing *)
    match no_show >|= value, updated_no_show >|= value with
    | Some true, (Some false | None) -> update_num_no_shows ~step:(-1) contact
    | (Some false | None), Some true -> update_num_no_shows ~step:1 contact
    | None, None | Some _, Some _ | Some _, None | None, Some false -> contact
  in
  let contact =
    let open Participated in
    match
      ( participated_in_other_assignments
      , participated >|= value
      , updated_participated >|= value )
    with
    | true, _, _ -> contact
    | false, (Some false | None), Some true ->
      update_num_participations ~step:1 contact
    | false, Some true, (Some false | None) ->
      update_num_participations ~step:(-1) contact
    | _ -> contact
  in
  contact
;;
