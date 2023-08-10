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

let assignment_creation_and_confirmation_events
  experiment
  confirmation_email
  session
  follow_up_sessions
  contact
  =
  let open CCResult in
  let open Assignment in
  let all_sessions = session :: follow_up_sessions in
  let* (_ : unit list) =
    all_sessions |> CCList.map Session.assignment_creatable |> CCList.all_ok
  in
  let follow_up_events =
    follow_up_sessions
    |> CCList.map (fun session -> Created (create contact, session.Session.id))
  in
  let main_assignment = create contact in
  let confirmation_email = confirmation_email main_assignment in
  let email_event =
    Email.Sent (confirmation_email, experiment.Experiment.smtp_auth_id)
    |> Pool_event.email
  in
  let create_events =
    Created (main_assignment, session.Session.id) :: follow_up_events
    |> CCList.map Pool_event.assignment
  in
  Ok (email_event :: create_events)
;;

module Create : sig
  include Common.CommandSig

  type t =
    { contact : Contact.t
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; experiment : Experiment.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Assignment.t -> Sihl_email.t)
    -> bool
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { contact : Contact.t
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; experiment : Experiment.t
    }

  let handle
    ?(tags = Logs.Tag.empty)
    { contact; session; follow_up_sessions; experiment }
    confirmation_email
    already_enrolled
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let all_sessions = session :: follow_up_sessions in
    if already_enrolled
    then Error Pool_common.Message.(AlreadySignedUpForExperiment)
    else
      let* () =
        let open Experiment in
        (experiment.direct_registration_disabled
         |> DirectRegistrationDisabled.value
         || experiment.registration_disabled |> RegistrationDisabled.value)
        |> Utils.bool_to_result_not
             Pool_common.Message.(DirectRegistrationIsDisabled)
      in
      let* creation_events =
        assignment_creation_and_confirmation_events
          experiment
          confirmation_email
          session
          follow_up_sessions
          contact
      in
      let contact_event =
        Contact_counter.update_on_session_signup contact all_sessions
        |> Contact.updated
        |> Pool_event.contact
      in
      Ok (creation_events @ [ contact_event ])
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
    * Assignment.t list option
    * Assignment.ExternalDataId.t option)
    list

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
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
    * Assignment.t list option
    * Assignment.ExternalDataId.t option)
    list

  let handle
    ?(tags = Logs.Tag.empty)
    experiment
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
               , follow_ups
               , external_data_id ) ->
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
        let* external_data_id =
          match
            Experiment.external_data_required_value experiment, external_data_id
          with
          | true, None ->
            Error Pool_common.Message.(FieldRequired Field.ExternalDataId)
          | _, _ -> Ok external_data_id
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
        @ ((Assignment.AttendanceSet
              (assignment, no_show, participated, external_data_id)
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
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Assignment.t -> Sihl_email.t)
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { experiment : Experiment.t
    ; session : Session.t
    ; follow_up_sessions : Session.t list
    ; waiting_list : Waiting_list.t
    ; already_enrolled : bool
    }

  let handle
    ?(tags = Logs.Tag.empty)
    ({ experiment; session; follow_up_sessions; waiting_list; already_enrolled } :
      t)
    confirmation_email
    =
    let all_sessions = session :: follow_up_sessions in
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
      let contact = waiting_list.Waiting_list.contact in
      let* creation_events =
        assignment_creation_and_confirmation_events
          experiment
          confirmation_email
          session
          follow_up_sessions
          contact
      in
      let conter_events =
        Contact_counter.update_on_assignment_from_waiting_list
          contact
          all_sessions
        |> Contact.updated
        |> Pool_event.contact
      in
      Ok (creation_events @ [ conter_events ])
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
