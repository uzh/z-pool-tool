module Conformist = Pool_common.Utils.PoolConformist

module Create : sig
  type t =
    { contact : Contact.t
    ; session : Session.Public.t
    ; waiting_list : Waiting_list.t option
    ; experiment : Experiment.Public.t
    }

  val handle
    :  t
    -> Email.confirmation_email
    -> bool
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { contact : Contact.t
    ; session : Session.Public.t
    ; waiting_list : Waiting_list.t option
    ; experiment : Experiment.Public.t
    }

  let handle (command : t) confirmation_email already_enrolled =
    let open CCResult in
    if already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        match
          command.experiment.Experiment.Public.direct_registration_disabled
          |> Experiment.DirectRegistrationDisabled.value
        with
        | true -> Error Pool_common.Message.(DirectRegistrationIsDisabled)
        | false -> Ok ()
      in
      let* _ =
        Session.Public.is_fully_booked command.session
        |> function
        | true -> Error Pool_common.Message.(SessionFullyBooked)
        | false -> Ok ()
      in
      let create =
        Assignment.
          { contact = command.contact
          ; session_id = command.session.Session.Public.id
          }
      in
      let delete_events =
        match command.waiting_list with
        | None -> []
        | Some waiting_list ->
          [ Waiting_list.Deleted waiting_list |> Pool_event.waiting_list ]
      in
      Ok
        (delete_events
        @ [ Assignment.Created create |> Pool_event.assignment
          ; Contact.NumAssignmentsIncreased command.contact
            |> Pool_event.contact
          ; Email.AssignmentConfirmationSent
              (command.contact.Contact.user, confirmation_email)
            |> Pool_event.email
          ])
  ;;

  let effects = [ `Create, `TargetEntity `Assignment ]
end

module Cancel : sig
  type t = Assignment.t

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : t -> Guard.Authorizer.effect list
end = struct
  type t = Assignment.t

  let handle (command : t)
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    Ok
      [ Assignment.Canceled command |> Pool_event.assignment
      ; Contact.NumAssignmentsDecreased command.Assignment.contact
        |> Pool_event.contact
      ]
  ;;

  let effects command =
    [ ( `Update
      , `Target
          (command.Assignment.id |> Guard.Uuid.target_of Pool_common.Id.value) )
    ]
  ;;
end

let validate_participation ((_, show_up, participated) as participation) =
  let open Assignment in
  if Participated.value participated && not (ShowUp.value show_up)
  then
    Error
      Pool_common.Message.(FieldRequiresCheckbox Field.(Participated, ShowUp))
  else Ok participation
;;

module SetAttendance : sig
  type t = (Assignment.t * Assignment.ShowUp.t * Assignment.Participated.t) list

  val handle
    :  Session.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Assignment.t -> Guard.Authorizer.effect list
end = struct
  type t = (Assignment.t * Assignment.ShowUp.t * Assignment.Participated.t) list

  let handle (session : Session.t) command =
    let open CCResult in
    let open Assignment in
    let open Session in
    let* () =
      if CCOption.is_some session.closed_at
      then Error Pool_common.Message.SessionAlreadyClosed
      else Ok ()
    in
    let* () =
      if Ptime.is_earlier
           (session.start |> Start.value)
           ~than:Ptime_clock.(now ())
      then Ok ()
      else Error Pool_common.Message.SessionNotStarted
    in
    CCList.fold_left
      (fun events participation ->
        events
        >>= fun events ->
        participation
        |> validate_participation
        >|= fun ((assignment : Assignment.t), showup, participated) ->
        let contact_event =
          let open Contact in
          let update =
            { show_up = ShowUp.value showup
            ; participated = Participated.value participated
            }
          in
          SessionParticipationSet (assignment.contact, update)
          |> Pool_event.contact
        in
        events
        @ [ Assignment.AttendanceSet (assignment, showup, participated)
            |> Pool_event.assignment
          ; contact_event
          ])
      (Ok [ Closed session |> Pool_event.session ])
      command
  ;;

  let effects assignment =
    [ ( `Update
      , `Target
          (assignment.Assignment.id |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end

module CreateFromWaitingList : sig
  type t =
    { session : Session.t
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  val handle
    :  t
    -> Email.confirmation_email
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { session : Session.t
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  let handle (command : t) confirmation_email =
    let open CCResult in
    if command.already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        Session.is_fully_booked command.session
        |> function
        | true -> Error Pool_common.Message.(SessionFullyBooked)
        | false -> Ok ()
      in
      match
        command.waiting_list.Waiting_list.experiment
        |> Experiment.registration_disabled_value
      with
      | true -> Error Pool_common.Message.(RegistrationDisabled)
      | false ->
        let create =
          let open Waiting_list in
          Assignment.
            { contact = command.waiting_list.contact
            ; session_id = command.session.Session.id
            }
        in
        Ok
          [ Waiting_list.Deleted command.waiting_list |> Pool_event.waiting_list
          ; Assignment.Created create |> Pool_event.assignment
          ; Email.AssignmentConfirmationSent
              ( command.waiting_list.Waiting_list.contact.Contact.user
              , confirmation_email )
            |> Pool_event.email
          ]
  ;;

  let effects = [ `Create, `TargetEntity `Assignment ]
end
