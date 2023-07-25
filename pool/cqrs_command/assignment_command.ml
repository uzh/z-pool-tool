module Conformist = Pool_common.Utils.PoolConformist
open CCFun.Infix

let src = Logs.Src.create "assignment.cqrs"

let assignment_effect action id =
  let open Guard in
  ValidationSet.One
    ( action
    , TargetSpec.Id (`Assignment, id |> Guard.Uuid.target_of Assignment.Id.value)
    )
;;

module Create : sig
  include Common.CommandSig

  type t =
    { contact : Contact.t
    ; sessions : Session.Public.t list
    ; experiment : Experiment.t
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
    ; experiment : Experiment.t
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
        let open Experiment in
        (command.experiment.direct_registration_disabled
         |> DirectRegistrationDisabled.value
         || command.experiment.registration_disabled
            |> RegistrationDisabled.value)
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
      let contact_event =
        Contact_counter.update_on_session_signup
          command.contact
          command.sessions
        |> Contact.updated
        |> Pool_event.contact
      in
      Ok
        (create_events
         @ [ contact_event
           ; Email.Sent
               (confirmation_email, command.experiment.Experiment.smtp_auth_id)
             |> Pool_event.email
           ])
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
      let* () = Session.assignments_cancelable session in
      CCList.map Assignment.is_cancellable assignments |> CCList.all_ok
    in
    let cancel_events =
      CCList.map
        (fun assignment ->
          Assignment.Canceled assignment |> Pool_event.assignment)
        assignments
    in
    let decrease_assignment_count =
      Contact_counter.update_on_assignment_cancellation assignments contact
      |> Contact.updated
      |> Pool_event.contact
    in
    Ok (cancel_events @ [ decrease_assignment_count ])
  ;;

  let effects = Assignment.Guard.Access.delete
end

module SetAttendance : sig
  type t =
    (Assignment.t
    * Assignment.NoShow.t
    * Assignment.Participated.t
    * Assignment.IncrementParticipationCount.t
    * Assignment.t list option)
    list

  val handle
    :  ?tags:Logs.Tag.set
    -> Session.t
    -> Tags.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    (Assignment.t
    * Assignment.NoShow.t
    * Assignment.Participated.t
    * Assignment.IncrementParticipationCount.t
    * Assignment.t list option)
    list

  let handle
    ?(tags = Logs.Tag.empty)
    (session : Session.t)
    (participation_tags : Tags.t list)
    (command : t)
    =
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
        |> fun ( ({ contact; _ } as assignment : Assignment.t)
               , no_show
               , participated
               , increment_num_participaton
               , follow_ups ) ->
        let cancel_followups =
          NoShow.value no_show || not (Participated.value participated)
        in
        let* () = attendance_settable assignment in
        let* contact =
          Contact_counter.update_on_session_closing
            contact
            no_show
            participated
            increment_num_participaton
        in
        let num_assignments_decrement, mark_as_deleted =
          let open CCList in
          match cancel_followups, follow_ups with
          | true, Some follow_ups ->
            let num_assignments =
              follow_ups
              |> filter (fun assignment ->
                   CCOption.is_none assignment.Assignment.canceled_at)
                 %> length
            in
            let marked_as_deleted =
              follow_ups >|= markedasdeleted %> Pool_event.assignment
            in
            num_assignments, marked_as_deleted
          | _, _ -> 0, []
        in
        let contact =
          Contact.update_num_assignments
            ~step:(CCInt.neg num_assignments_decrement)
            contact
        in
        let tag_events =
          let open Tags in
          match participated |> Participated.value with
          | false -> []
          | true ->
            participation_tags
            |> CCList.map (fun (tag : t) ->
              Tagged
                Tagged.{ model_uuid = Contact.id contact; tag_uuid = tag.id }
              |> Pool_event.tags)
        in
        let contact_events =
          (Contact.Updated contact |> Pool_event.contact) :: mark_as_deleted
        in
        events
        @ ((Assignment.AttendanceSet (assignment, no_show, participated)
            |> Pool_event.assignment)
           :: contact_events)
        @ tag_events
        |> CCResult.return)
      (Ok [ Closed session |> Pool_event.session ])
      command
  ;;

  let effects = Session.Guard.Access.update
end

module CreateFromWaitingList : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; sessions : Session.t list
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
    { experiment : Experiment.t
    ; sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  let handle
    ?(tags = Logs.Tag.empty)
    ({ experiment; sessions; waiting_list; already_enrolled } : t)
    confirmation_email
    =
    Logs.info ~src (fun m -> m "Handle command CreateFromWaitingList" ~tags);
    let open CCResult in
    if already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        waiting_list.Waiting_list.experiment
        |> Experiment.registration_disabled_value
        |> Utils.bool_to_result_not Pool_common.Message.(RegistrationDisabled)
      in
      let* (_ : unit list) =
        sessions |> CCList.map Session.assignment_creatable |> CCList.all_ok
      in
      let contact = waiting_list.Waiting_list.contact in
      let create_events =
        sessions
        |> CCList.map (fun session ->
          let create =
            Assignment.{ contact; session_id = session.Session.id }
          in
          Assignment.Created create |> Pool_event.assignment)
      in
      Ok
        (create_events
         @ [ Contact_counter.update_on_assignment_from_waiting_list
               contact
               sessions
             |> Contact.updated
             |> Pool_event.contact
           ; Email.Sent (confirmation_email, experiment.Experiment.smtp_auth_id)
             |> Pool_event.email
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
  include
    Common.CommandSig
      with type t =
        Contact.t * Assignment.t list * Assignment.IncrementParticipationCount.t

  val effects : Experiment.Id.t -> Assignment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    Contact.t * Assignment.t list * Assignment.IncrementParticipationCount.t

  let handle
    ?(tags = Logs.Tag.empty)
    (contact, assignments, decrement_participation_count)
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open Assignment in
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command MarkAsDeleted");
    let* (_ : unit list) =
      CCList.map is_deletable assignments |> CCList.all_ok
    in
    let mark_as_deleted =
      CCList.map (markedasdeleted %> Pool_event.assignment) assignments
    in
    let contact_updated =
      Contact_counter.update_on_assignment_deletion
        assignments
        contact
        decrement_participation_count
      |> Contact.updated
      |> Pool_event.contact
    in
    Ok (contact_updated :: mark_as_deleted)
  ;;

  let effects = Assignment.Guard.Access.delete
end
