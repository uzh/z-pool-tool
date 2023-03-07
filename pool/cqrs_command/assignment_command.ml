module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "assignment.cqrs"

module Create : sig
  include Common.CommandSig

  type t =
    { contact : Contact.t
    ; session : Session.Public.t
    ; waiting_list : Waiting_list.t option
    ; experiment : Experiment.Public.t
    ; follow_ups : Session.Public.t list option
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Sihl_email.t
    -> bool
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t =
    { contact : Contact.t
    ; session : Session.Public.t
    ; waiting_list : Waiting_list.t option
    ; experiment : Experiment.Public.t
    ; follow_ups : Session.Public.t list option
    }

  let handle
    ?(tags = Logs.Tag.empty)
    (command : t)
    confirmation_email
    already_enrolled
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    if already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else (
      let session_list =
        command.session :: CCOption.value ~default:[] command.follow_ups
      in
      let* () =
        match
          command.experiment.Experiment.Public.direct_registration_disabled
          |> Experiment.DirectRegistrationDisabled.value
        with
        | true -> Error Pool_common.Message.(DirectRegistrationIsDisabled)
        | false -> Ok ()
      in
      let* () =
        CCList.fold_left
          (fun res session ->
            res
            >>= fun () ->
            Session.Public.is_fully_booked session
            |> function
            | true -> Error Pool_common.Message.(SessionFullyBooked)
            | false -> Ok ())
          (CCResult.return ())
          session_list
      in
      let create_events =
        session_list
        |> CCList.map (fun session ->
             let create =
               Assignment.
                 { contact = command.contact
                 ; session_id = session.Session.Public.id
                 }
             in
             Assignment.Created create |> Pool_event.assignment)
      in
      let delete_events =
        match command.waiting_list with
        | None -> []
        | Some waiting_list ->
          [ Waiting_list.Deleted waiting_list |> Pool_event.waiting_list ]
      in
      let increase_num_events =
        Contact.NumAssignmentsIncreasedBy
          (command.contact, CCList.length session_list)
        |> Pool_event.contact
      in
      Ok
        (delete_events
         @ create_events
         @ [ increase_num_events ]
         @ [ Email.Sent confirmation_email |> Pool_event.email ]))
  ;;

  let effects = [ `Create, `TargetEntity `Assignment ]
end

module Cancel : sig
  include Common.CommandSig with type t = Assignment.t * Session.t

  val effects : Assignment.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Assignment.t * Session.t

  let handle ?(tags = Logs.Tag.empty) (assignment, session)
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Cancel" ~tags);
    let* () = Session.assignments_cancelable session in
    let* () = Assignment.is_cancellable assignment in
    Ok
      [ Assignment.Canceled assignment |> Pool_event.assignment
      ; Contact.NumAssignmentsDecreasedBy (assignment.Assignment.contact, 1)
        |> Pool_event.contact
      ]
  ;;

  let effects id =
    [ `Delete, `Target (id |> Guard.Uuid.target_of Assignment.Id.value)
    ; `Delete, `TargetEntity `Assignment
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
    :  ?tags:Logs.Tag.set
    -> Session.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_common.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = (Assignment.t * Assignment.ShowUp.t * Assignment.Participated.t) list

  let handle ?(tags = Logs.Tag.empty) (session : Session.t) command =
    Logs.info ~src (fun m -> m "Handle command SetAttendance" ~tags);
    let open CCResult in
    let open Assignment in
    let open Session in
    let* () = Session.is_closable session in
    CCList.fold_left
      (fun events participation ->
        events
        >>= fun events ->
        participation
        |> validate_participation
        >>= fun ((assignment : Assignment.t), showup, participated) ->
        let* contact_event =
          let open Contact in
          let* () = Assignment.attendance_settable assignment in
          let update =
            { show_up = ShowUp.value showup
            ; participated = Participated.value participated
            }
          in
          SessionParticipationSet (assignment.contact, update)
          |> Pool_event.contact
          |> CCResult.return
        in
        events
        @ [ Assignment.AttendanceSet (assignment, showup, participated)
            |> Pool_event.assignment
          ; contact_event
          ]
        |> CCResult.return)
      (Ok [ Closed session |> Pool_event.session ])
      command
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Pool_common.Id.value)
    ; `Update, `TargetEntity `Assignment
    ]
  ;;
end

module CreateFromWaitingList : sig
  type t =
    { session : Session.t
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    ; follow_ups : Session.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Sihl_email.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { session : Session.t
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    ; follow_ups : Session.t list
    }

  let handle ?(tags = Logs.Tag.empty) (command : t) confirmation_email =
    Logs.info ~src (fun m -> m "Handle command CreateFromWaitingList" ~tags);
    let open CCResult in
    if command.already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        match
          command.waiting_list.Waiting_list.experiment
          |> Experiment.registration_disabled_value
        with
        | true -> Error Pool_common.Message.(RegistrationDisabled)
        | false -> Ok ()
      in
      let session_list = command.session :: command.follow_ups in
      let* () =
        CCList.fold_left
          (fun res session ->
            res
            >>= fun () ->
            Session.is_fully_booked session
            |> function
            | true -> Error Pool_common.Message.(SessionFullyBooked)
            | false -> Ok ())
          (CCResult.return ())
          session_list
      in
      let contact = command.waiting_list.Waiting_list.contact in
      let create_events =
        session_list
        |> CCList.map (fun session ->
             let create =
               Assignment.{ contact; session_id = session.Session.id }
             in
             Assignment.Created create |> Pool_event.assignment)
      in
      Ok
        (create_events
         @ [ Contact.NumAssignmentsIncreasedBy
               (contact, CCList.length session_list)
             |> Pool_event.contact
           ; Waiting_list.Deleted command.waiting_list
             |> Pool_event.waiting_list
           ; Email.Sent confirmation_email |> Pool_event.email
           ])
  ;;

  let effects =
    (* TODO: make sure effects are "AND" and not "OR" *)
    [ `Update, `TargetEntity `WaitingList; `Create, `TargetEntity `Assignment ]
  ;;
end

module MarkAsDeleted : sig
  include Common.CommandSig with type t = Assignment.t list

  val effects : Assignment.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Assignment.t list

  let handle ?(tags = Logs.Tag.empty) assignments
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command MarkAsDeleted");
    let* (_ : unit list) =
      CCList.map Assignment.is_deletable assignments |> CCList.all_ok
    in
    let mark_as_deleted =
      CCList.map
        (fun assignment ->
          Assignment.MarkedAsDeleted assignment |> Pool_event.assignment)
        assignments
    in
    let assignment_count_event =
      let open CCList in
      let assignment_list =
        filter
          (fun assignment -> CCOption.is_none assignment.Assignment.canceled_at)
          assignments
      in
      if length assignment_list > 0
      then
        Some
          (Contact.NumAssignmentsDecreasedBy
             ((hd assignment_list).Assignment.contact, length assignment_list)
           |> Pool_event.contact)
      else None
    in
    let events =
      match assignment_count_event with
      | Some event -> event :: mark_as_deleted
      | None -> mark_as_deleted
    in
    Ok events
  ;;

  let effects id =
    [ `Delete, `Target (id |> Guard.Uuid.target_of Assignment.Id.value)
    ; `Delete, `TargetEntity `Assignment
    ]
  ;;
end
