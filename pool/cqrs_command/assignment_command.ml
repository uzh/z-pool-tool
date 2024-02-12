module Conformist = Pool_common.Utils.PoolConformist
open CCFun.Infix

let src = Logs.Src.create "assignment.cqrs"

type update =
  { no_show : Assignment.NoShow.t
  ; participated : Assignment.Participated.t
  ; external_data_id : Assignment.ExternalDataId.t option
  }

let update_command no_show participated external_data_id =
  { no_show; participated; external_data_id }
;;

let update_schema =
  let open Assignment in
  Conformist.(
    make
      Field.
        [ NoShow.schema ()
        ; Participated.schema ()
        ; Conformist.optional @@ ExternalDataId.schema ()
        ]
      update_command)
;;

let decode_update data =
  Conformist.decode_and_validate update_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

let assignment_effect action uuid =
  let target_id = uuid |> Guard.Uuid.target_of Assignment.Id.value in
  Guard.ValidationSet.one_of_tuple (action, `Assignment, Some target_id)
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
    -> ?direct_enrollment_by_admin:bool
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
    ?(direct_enrollment_by_admin = false)
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
        if direct_enrollment_by_admin
        then Ok ()
        else
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

  let effects = Assignment.Guard.Access.update
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
    let open Guard.ValidationSet in
    And
      [ Waiting_list.Guard.Access.update experiment_id waiting_list_id
      ; Assignment.Guard.Access.create experiment_id
      ]
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

module Update : sig
  type t = update

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> bool
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t = update

  let handle
    ?(tags = Logs.Tag.empty)
    (experiment : Experiment.t)
    ({ Session.closed_at; _ } as session)
    ({ Assignment.no_show; participated; _ } as assignment)
    participated_in_other_assignments
    (command : update)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let open Assignment in
    let contact_counters =
      match CCOption.is_some closed_at, no_show, participated with
      | true, Some no_show, Some _ ->
        Contact_counter.update_on_assignment_update
          assignment
          session
          no_show
          command.no_show
          participated_in_other_assignments
        |> Contact.updated
        |> Pool_event.contact
        |> CCList.return
      | _ -> []
    in
    let updated_assignment =
      { assignment with
        no_show = Some command.no_show
      ; participated = Some command.participated
      ; external_data_id = command.external_data_id
      }
    in
    let* () =
      validate experiment updated_assignment
      |> function
      | Ok () | Error [] -> Ok ()
      | Error (hd :: _) -> Error hd
    in
    Ok
      ((Assignment.Updated updated_assignment |> Pool_event.assignment)
       :: contact_counters)
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module SendReminder : sig
  include Common.CommandSig with type t = Pool_common.Reminder.Channel.t

  val handle
    :  ?tags:Logs.Tag.set
    -> (Assignment.t -> (Sihl_email.t, Pool_common.Message.error) result)
       * (Assignment.t
          -> Pool_user.CellPhone.t
          -> (Text_message.t, Pool_common.Message.error) result)
    -> Experiment.t
    -> Session.t
    -> Assignment.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode : Conformist.input -> (t, Conformist.error_msg) result
  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_common.Reminder.Channel.t

  let schema =
    Conformist.(make Field.[ Pool_common.Reminder.Channel.schema () ] CCFun.id)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    (create_email, create_text_message)
    experiment
    session
    ({ Assignment.contact; _ } as assignment)
    channel
    =
    Logs.info ~src (fun m -> m "Handle command ResendReminders" ~tags);
    let open Pool_common.Reminder in
    let open CCResult.Infix in
    let create_email assignment =
      assignment
      |> create_email
      >|= fun email -> email, experiment.Experiment.smtp_auth_id
    in
    let* () = Assignment.reminder_sendable session assignment in
    let* msg_event =
      let open Channel in
      match channel with
      | Email -> assignment |> create_email >|= Email.sent >|= Pool_event.email
      | TextMessage ->
        (match contact.Contact.cell_phone with
         | None -> Error Pool_common.Message.(Missing Field.CellPhone)
         | Some cell_phone ->
           create_text_message assignment cell_phone
           >|= Text_message.sent
           >|= Pool_event.text_message)
    in
    let update =
      Assignment.(
        Updated
          { assignment with
            reminder_manually_last_sent_at = Some (SentAt.create_now ())
          })
      |> Pool_event.assignment
    in
    Ok [ msg_event; update ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects exp_id = Session.Guard.Access.update exp_id
end

type session_swap =
  { session : Session.Id.t
  ; notify_contact : Pool_common.NotifyContact.t
  ; language : Pool_common.Language.t
  ; email_subject : Message_template.EmailSubject.t
  ; email_text : Message_template.EmailText.t
  ; plain_text : Message_template.PlainText.t
  }

let session_schema () =
  let open Session in
  Pool_common.Utils.schema_decoder
    CCFun.(Id.of_string %> CCResult.return)
    Id.value
    Pool_common.Message.Field.Session
;;

module SwapSession : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> ?assignment_id:Assignment.Id.t
    -> current_session:Session.t
    -> new_session:Session.t
    -> Assignment.t
    -> (Sihl_email.t * Email.SmtpAuth.Id.t option) option
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (session_swap, Pool_common.Message.error) result
end = struct
  let handle
    ?(tags = Logs.Tag.empty)
    ?assignment_id
    ~current_session
    ~new_session
    assignment
    notification_email
    : (Pool_event.t list, Pool_common.Message.error) result
    =
    let open Assignment in
    let open CCResult in
    Logs.info ~src (fun m -> m ~tags "Handle command SwapSession");
    let* () = Session.can_be_assigned_to_existing_assignment new_session in
    let* () = session_changeable current_session assignment in
    let new_assignment =
      { assignment with
        id = CCOption.value ~default:(Assignment.Id.create ()) assignment_id
      }
    in
    let email_event =
      notification_email
      |> CCOption.map_or
           ~default:[]
           CCFun.(Email.sent %> Pool_event.email %> CCList.return)
    in
    Ok
      ([ MarkedAsDeleted assignment |> Pool_event.assignment
       ; Created (new_assignment, new_session.Session.id)
         |> Pool_event.assignment
       ]
       @ email_event)
  ;;

  let command
    session
    notify_contact
    language
    email_subject
    email_text
    plain_text
    =
    { session; notify_contact; language; email_subject; email_text; plain_text }
  ;;

  let schema =
    let open Message_template in
    Pool_common.Utils.PoolConformist.(
      make
        Field.
          [ session_schema ()
          ; Pool_common.NotifyContact.schema ()
          ; Pool_common.Language.schema ()
          ; EmailSubject.schema ()
          ; EmailText.schema ()
          ; PlainText.schema ()
          ]
        command)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end
