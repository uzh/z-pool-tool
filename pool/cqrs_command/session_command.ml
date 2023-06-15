module Conformist = Pool_common.Utils.PoolConformist
open CCFun

let src = Logs.Src.create "session.cqrs"

let create_command
  start
  duration
  description
  limitations
  max_participants
  min_participants
  overbook
  reminder_lead_time
  : Session.base
  =
  Session.
    { start
    ; duration
    ; description
    ; limitations
    ; max_participants
    ; min_participants
    ; overbook
    ; reminder_lead_time
    }
;;

let create_schema =
  Conformist.(
    make
      Field.
        [ Session.Start.schema ()
        ; Session.Duration.schema ()
        ; Conformist.optional @@ Session.Description.schema ()
        ; Conformist.optional @@ Session.Limitations.schema ()
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MaxParticipants
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MinParticipants
        ; Session.ParticipantAmount.schema Pool_common.Message.Field.Overbook
        ; Conformist.optional @@ Pool_common.Reminder.LeadTime.schema ()
        ]
      create_command)
;;

let update_command
  start
  duration
  description
  limitations
  max_participants
  min_participants
  overbook
  reminder_lead_time
  : Session.update
  =
  Session.
    { start
    ; duration
    ; description
    ; limitations
    ; max_participants
    ; min_participants
    ; overbook
    ; reminder_lead_time
    }
;;

let update_schema =
  Conformist.(
    make
      Field.
        [ Conformist.optional @@ Session.Start.schema ()
        ; Conformist.optional @@ Session.Duration.schema ()
        ; Conformist.optional @@ Session.Description.schema ()
        ; Conformist.optional @@ Session.Limitations.schema ()
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MaxParticipants
        ; Session.ParticipantAmount.schema
            Pool_common.Message.Field.MinParticipants
        ; Session.ParticipantAmount.schema Pool_common.Message.Field.Overbook
        ; Conformist.optional @@ Pool_common.Reminder.LeadTime.schema ()
        ]
      update_command)
;;

(* If session is follow-up, make sure it's later than parent *)
let starts_after_parent parent_session start =
  let open Session in
  CCOption.map_or
    ~default:false
    (fun (s : Session.t) ->
      Ptime.is_earlier ~than:(Start.value s.start) (Start.value start))
    parent_session
;;

let validate_start follow_up_sessions parent_session start =
  let open Session in
  (* If session has follow-ups, make sure they are all later *)
  let starts_before_followups =
    CCList.exists
      (fun (follow_up : Session.t) ->
        Ptime.is_earlier ~than:(Start.value start) (Start.value follow_up.start))
      follow_up_sessions
  in
  if starts_after_parent parent_session start || starts_before_followups
  then Error Pool_common.Message.FollowUpIsEarlierThanMain
  else Ok ()
;;

let run_validations validations =
  let open CCResult in
  validations
  |> CCList.filter fst
  |> CCList.map (fun (_, err) -> Error err)
  |> flatten_l
  |> map ignore
;;

module Create : sig
  include Common.CommandSig with type t = Session.base

  val handle
    :  ?tags:Logs.Tag.set
    -> ?parent_session:Session.t
    -> ?session_id:Session.Id.t
    -> Experiment.Id.t
    -> Pool_location.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode : Conformist.input -> (t, Conformist.error_msg) result
  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Session.base

  let schema = create_schema

  let handle
    ?(tags = Logs.Tag.empty)
    ?parent_session
    ?session_id
    experiment_id
    location
    (Session.
       { start
       ; duration
       ; description
       ; limitations
       ; max_participants
       ; min_participants
       ; (* TODO [aerben] find a better name *)
         overbook
       ; reminder_lead_time
       } :
      Session.base)
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let validations =
      [ ( starts_after_parent parent_session start
        , Pool_common.Message.FollowUpIsEarlierThanMain )
      ; ( max_participants < min_participants
        , Pool_common.Message.(
            Smaller (Field.MaxParticipants, Field.MinParticipants)) )
      ]
    in
    let* () = run_validations validations in
    let session =
      Session.create
        ?id:session_id
        ?follow_up_to:(parent_session |> CCOption.map (fun s -> s.Session.id))
        start
        duration
        description
        limitations
        location
        max_participants
        min_participants
        overbook
        reminder_lead_time
    in
    Ok [ Session.Created (session, experiment_id) |> Pool_event.session ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Session.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = Session.update

  val handle
    :  ?tags:Logs.Tag.set
    -> ?parent_session:Session.t
    -> Session.t list
    -> Session.t
    -> Pool_location.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Session.update

  let handle
    ?(tags = Logs.Tag.empty)
    ?parent_session
    follow_up_sessions
    session
    location
    (Session.
       { start
       ; duration
       ; description
       ; limitations
       ; max_participants
       ; min_participants
       ; overbook
       ; reminder_lead_time
       } :
      Session.update)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open Session in
    let open CCResult in
    let has_assignments = Session.has_assignments session in
    let* start, duration =
      let open Pool_common.Message in
      let to_result field =
        CCOption.to_result (Conformist [ field, Pool_common.Message.NoValue ])
      in
      match has_assignments with
      | false ->
        CCResult.both
          (to_result Field.Start start)
          (to_result Field.Duration duration)
      | true -> Ok (session.start, session.duration)
    in
    let* () = validate_start follow_up_sessions parent_session start in
    let* () =
      if max_participants < min_participants
      then
        Error
          Pool_common.Message.(
            Smaller (Field.MaxParticipants, Field.MinParticipants))
      else Ok ()
    in
    let (session_cmd : Session.base) =
      Session.
        { start
        ; duration
        ; description
        ; limitations
        ; max_participants
        ; min_participants
        ; overbook
        ; reminder_lead_time
        }
    in
    Ok
      [ Session.Updated (session_cmd, location, session) |> Pool_event.session ]
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Session.Guard.Access.update
end

module Reschedule : sig
  include Common.CommandSig with type t = Session.reschedule

  val handle
    :  ?tags:Logs.Tag.set
    -> ?parent_session:Session.t
    -> Session.t list
    -> Session.t
    -> Assignment.t list
    -> (Contact.t
        -> Session.Start.t
        -> Session.Duration.t
        -> (Sihl_email.t, Pool_common.Message.error) result)
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Session.reschedule

  let command start duration : Session.reschedule = Session.{ start; duration }

  let schema =
    Conformist.(
      make Field.[ Session.Start.schema (); Session.Duration.schema () ] command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?parent_session
    follow_up_sessions
    session
    assignments
    create_message
    (Session.{ start; duration } as reschedule : Session.reschedule)
    =
    Logs.info ~src (fun m -> m "Handle command Reschedule" ~tags);
    let open CCResult in
    let* () = validate_start follow_up_sessions parent_session start in
    let* () =
      if Ptime.is_earlier
           ~than:(Ptime_clock.now ())
           (start |> Session.Start.value)
      then Error Pool_common.Message.TimeInPast
      else Ok ()
    in
    let* emails =
      let open Assignment in
      assignments
      |> CCList.map (fun ({ contact; _ } : t) ->
           create_message contact start duration)
      |> CCResult.flatten_l
    in
    Ok
      ((Session.Rescheduled (session, reschedule) |> Pool_event.session)
       ::
       (if emails |> CCList.is_empty |> not
        then [ Email.BulkSent emails |> Pool_event.email ]
        else []))
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Session.Guard.Access.update
end

module Delete : sig
  include Common.CommandSig

  type t =
    { session : Session.t
    ; follow_ups : Session.t list
    ; templates : Message_template.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { session : Session.t
    ; follow_ups : Session.t list
    ; templates : Message_template.t list
    }

  let handle ?(tags = Logs.Tag.empty) { session; follow_ups; templates } =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    if CCList.is_empty follow_ups |> not
    then Error Pool_common.Message.SessionHasFollowUps
    else
      let* () = Session.is_deletable session follow_ups in
      let delete_template =
        Message_template.deleted %> Pool_event.message_template
      in
      Ok
        ((Session.Deleted session |> Pool_event.session)
         :: (templates |> CCList.map delete_template))
  ;;

  let effects = Session.Guard.Access.delete
end

module Cancel : sig
  type t = Session.CancellationReason.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Session.t list
    -> (Contact.t * Assignment.t list) list
    -> (t -> Contact.t -> (Sihl_email.t, Pool_common.Message.error) result)
    -> (t
        -> Contact.t
        -> Pool_user.CellPhone.t
        -> (Text_message.t, Pool_common.Message.error) result)
    -> Pool_common.NotifyVia.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode : Conformist.input -> (t, Conformist.error_msg) result
  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Session.CancellationReason.t

  let handle
    ?(tags = Logs.Tag.empty)
    sessions
    (assignments : (Contact.t * Assignment.t list) list)
    email_fn
    text_message_fn
    notify_via
    (reason : t)
    =
    Logs.info ~src (fun m -> m "Handle command Cancel" ~tags);
    let open CCResult in
    let* (_ : unit list) =
      sessions |> CCList.map Session.is_cancellable |> CCList.all_ok
    in
    let* (_ : unit list) =
      let open CCList in
      assignments >|= snd |> flatten >|= Assignment.is_not_closed |> all_ok
    in
    let contact_events =
      assignments
      |> CCList.map (fun (contact, assignments) ->
           contact
           |> Contact_counter.update_on_session_cancellation assignments
           |> Contact.updated
           |> Pool_event.contact)
    in
    let* notification_events =
      let open Pool_common.NotifyVia in
      let email_event contact =
        email_fn reason contact >|= Email.sent >|= Pool_event.email
      in
      let text_msg_event contact phone_number =
        text_message_fn reason contact phone_number
        >|= Text_message.sent
        >|= Pool_event.text_message
      in
      let fallback_to_email = CCList.mem ~eq:equal Email notify_via |> not in
      match notify_via with
      | [] -> Error Pool_common.Message.(NoOptionSelected Field.NotifyVia)
      | notify_via ->
        notify_via
        |> CCList.flat_map (function
             | Email ->
               assignments
               |> CCList.map (fun (contact, _) -> email_event contact)
             | TextMessage ->
               assignments
               |> CCList.filter_map (fun (contact, _) ->
                    match contact.Contact.cell_phone, fallback_to_email with
                    | Some phone_number, (true | false) ->
                      text_msg_event contact phone_number |> CCOption.return
                    | None, true -> email_event contact |> CCOption.return
                    | _, _ -> None))
        |> CCList.all_ok
    in
    let cancel_events =
      sessions
      |> CCList.map (fun session ->
           Session.Canceled session |> Pool_event.session)
    in
    [ notification_events; cancel_events; contact_events ]
    |> CCList.flatten
    |> CCResult.return
  ;;

  let schema =
    Conformist.(make Field.[ Session.CancellationReason.schema () ] CCFun.id)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Session.Guard.Access.update
end

module SendReminder : sig
  include Common.CommandSig with type t = (Session.t * Sihl_email.t list) list

  val effects : Experiment.Id.t -> Session.Id.t -> Guard.ValidationSet.t
end = struct
  type t = (Session.t * Sihl_email.t list) list

  let handle ?(tags = Logs.Tag.empty) command =
    Logs.info ~src (fun m -> m "Handle command SendReminder" ~tags);
    Ok
      (CCList.flat_map
         (fun (session, emails) ->
           (Session.ReminderSent session |> Pool_event.session)
           ::
           (if emails |> CCList.is_empty |> not
            then [ Email.BulkSent emails |> Pool_event.email ]
            else []))
         command)
  ;;

  let effects = Session.Guard.Access.update
end
