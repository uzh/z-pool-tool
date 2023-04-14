module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "assignment.cqrs"

let assignment_effect action id =
  let open Guard in
  ValidationSet.One
    ( action
    , TargetSpec.Id (`Assignment, id |> Guard.Uuid.target_of Assignment.Id.value)
    )
;;

let update_session_participation_counts
  ({ Contact.num_show_ups; num_no_shows; num_participations; _ } as contact)
  no_show
  participated
  =
  let open Contact in
  let open Assignment in
  let num_no_shows, num_show_ups =
    match NoShow.value no_show with
    | true -> num_no_shows |> NumberOfNoShows.increment, num_show_ups
    | false -> num_no_shows, num_show_ups |> NumberOfShowUps.increment
  in
  let num_participations =
    if Participated.value participated
    then num_participations |> NumberOfParticipations.increment
    else num_participations
  in
  { contact with num_no_shows; num_show_ups; num_participations }
;;

module Create : sig
  include Common.CommandSig

  type t =
    { contact : Contact.t
    ; sessions : Session.Public.t list
    ; experiment : Experiment.Public.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Sihl_email.t
    -> bool
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { contact : Contact.t
    ; sessions : Session.Public.t list
    ; experiment : Experiment.Public.t
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
    else
      let* () =
        command.experiment.Experiment.Public.direct_registration_disabled
        |> Experiment.DirectRegistrationDisabled.value
        |> Utils.bool_to_result_not
             Pool_common.Message.(DirectRegistrationIsDisabled)
      in
      let* (_ : unit list) =
        command.sessions
        |> CCList.map Session.Public.assignment_creatable
        |> CCList.all_ok
      in
      let create_events =
        command.sessions
        |> CCList.map (fun session ->
             let create =
               Assignment.
                 { contact = command.contact
                 ; session_id = session.Session.Public.id
                 }
             in
             Assignment.Created create |> Pool_event.assignment)
      in
      let increase_num_events =
        Contact.NumAssignmentsIncreasedBy
          (command.contact, CCList.length command.sessions)
        |> Pool_event.contact
      in
      Ok
        (create_events
         @ [ increase_num_events ]
         @ [ Email.Sent confirmation_email |> Pool_event.email ])
  ;;

  let effects = Assignment.Guard.Access.create
end

module Cancel : sig
  include Common.CommandSig with type t = Assignment.t list * Session.t

  val effects : Experiment.Id.t -> Assignment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Assignment.t list * Session.t

  let handle ?(tags = Logs.Tag.empty) (assignments, session)
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Cancel" ~tags);
    let contact =
      assignments
      |> CCList.hd
      |> fun ({ Assignment.contact; _ } : Assignment.t) -> contact
    in
    let* (_ : unit list) =
      CCList.map
        (fun assignment ->
          let* () = Session.assignments_cancelable session in
          let* () = Assignment.is_cancellable assignment in
          Ok ())
        assignments
      |> CCList.all_ok
    in
    let cancel_events =
      CCList.map
        (fun assignment ->
          Assignment.Canceled assignment |> Pool_event.assignment)
        assignments
    in
    let decrease_assignment_count =
      Contact.NumAssignmentsDecreasedBy (contact, CCList.length assignments)
      |> Pool_event.contact
    in
    Ok (cancel_events @ [ decrease_assignment_count ])
  ;;

  let effects = Assignment.Guard.Access.delete
end

let validate_participation ((_, no_show, participated, _) as participation) =
  let open Assignment in
  if Participated.value participated && NoShow.value no_show
  then
    Error Pool_common.Message.(MutuallyExclusive Field.(Participated, NoShow))
  else Ok participation
;;

module SetAttendance : sig
  type t =
    (Assignment.t
    * Assignment.NoShow.t
    * Assignment.Participated.t
    * Assignment.t list option)
    list

  val handle
    :  ?tags:Logs.Tag.set
    -> Session.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    (Assignment.t
    * Assignment.NoShow.t
    * Assignment.Participated.t
    * Assignment.t list option)
    list

  let handle ?(tags = Logs.Tag.empty) (session : Session.t) (command : t) =
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
        >>= fun ( ({ Assignment.contact; _ } as assignment : Assignment.t)
                , no_show
                , participated
                , follow_ups ) ->
        let* contact_events =
          let open Contact in
          let cancel_followups =
            NoShow.value no_show || not (Participated.value participated)
          in
          let* () = attendance_settable assignment in
          let contact =
            update_session_participation_counts contact no_show participated
          in
          let num_assignments, mark_as_deleted =
            let num_assignments = contact.num_assignments in
            match cancel_followups, follow_ups with
            | true, Some follow_ups ->
              let num_assignments =
                follow_ups
                |> CCFun.(
                     CCList.filter (fun assignment ->
                       CCOption.is_none assignment.Assignment.canceled_at)
                     %> CCList.length
                     %> NumberOfAssignments.decrement num_assignments)
              in
              let marked_as_deleted =
                follow_ups
                |> CCList.map (fun assignment ->
                     Assignment.MarkedAsDeleted assignment
                     |> Pool_event.assignment)
              in
              num_assignments, marked_as_deleted
            | _, _ -> num_assignments, []
          in
          let contact = { contact with num_assignments } in
          (Contact.Updated contact |> Pool_event.contact) :: mark_as_deleted
          |> CCResult.return
        in
        events
        @ ((Assignment.AttendanceSet (assignment, no_show, participated)
            |> Pool_event.assignment)
           :: contact_events)
        |> CCResult.return)
      (Ok [ Closed session |> Pool_event.session ])
      command
  ;;

  let effects = Session.Guard.Access.update
end

module CreateFromWaitingList : sig
  include Common.CommandSig

  type t =
    { sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Sihl_email.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  let handle ?(tags = Logs.Tag.empty) (command : t) confirmation_email =
    Logs.info ~src (fun m -> m "Handle command CreateFromWaitingList" ~tags);
    let open CCResult in
    if command.already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        command.waiting_list.Waiting_list.experiment
        |> Experiment.registration_disabled_value
        |> Utils.bool_to_result_not Pool_common.Message.(RegistrationDisabled)
      in
      let* (_ : unit list) =
        command.sessions
        |> CCList.map Session.assignment_creatable
        |> CCList.all_ok
      in
      let contact = command.waiting_list.Waiting_list.contact in
      let create_events =
        command.sessions
        |> CCList.map (fun session ->
             let create =
               Assignment.{ contact; session_id = session.Session.id }
             in
             Assignment.Created create |> Pool_event.assignment)
      in
      Ok
        (create_events
         @ [ Contact.NumAssignmentsIncreasedBy
               (contact, CCList.length command.sessions)
             |> Pool_event.contact
           ; Email.Sent confirmation_email |> Pool_event.email
           ])
  ;;

  let effects experiment_id waiting_list_id =
    let open Guard in
    ValidationSet.(
      And
        [ Waiting_list.Guard.Access.update experiment_id waiting_list_id
        ; Assignment.Guard.Access.create experiment_id
        ])
  ;;
end

module MarkAsDeleted : sig
  include Common.CommandSig with type t = Contact.t * Assignment.t list

  val effects : Experiment.Id.t -> Assignment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t * Assignment.t list

  let handle ?(tags = Logs.Tag.empty) (contact, assignments)
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open Assignment in
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command MarkAsDeleted");
    let* (_ : unit list) =
      CCList.map is_deletable assignments |> CCList.all_ok
    in
    let mark_as_deleted =
      CCList.map CCFun.(markedasdeleted %> Pool_event.assignment) assignments
    in
    let num_no_shows, num_show_ups, num_participations, num_assignments =
      let open Contact in
      assignments
      |> CCList.fold_left
           (fun (no_shows, show_ups, participations, assignment_count)
                (assignment : Assignment.t) ->
             let no_shows, show_ups =
               match assignment.no_show |> CCOption.map NoShow.value with
               | Some true -> NumberOfNoShows.decrement no_shows, show_ups
               | Some false -> no_shows, NumberOfShowUps.decrement show_ups
               | _ -> no_shows, show_ups
             in
             let assignment_count =
               if CCOption.is_some assignment.canceled_at
               then assignment_count
               else NumberOfAssignments.decrement assignment_count 1
             in
             let participations =
               NumberOfParticipations.decrement participations
             in
             no_shows, show_ups, participations, assignment_count)
           ( contact.num_no_shows
           , contact.num_show_ups
           , contact.num_participations
           , contact.num_assignments )
    in
    let contact_updated =
      let contact =
        Contact.
          { contact with
            num_no_shows
          ; num_show_ups
          ; num_participations
          ; num_assignments
          }
      in
      Contact.Updated contact |> Pool_event.contact
    in
    Ok (contact_updated :: mark_as_deleted)
  ;;

  let effects = Assignment.Guard.Access.delete
end
