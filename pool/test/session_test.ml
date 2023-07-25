module SessionC = Cqrs_command.Session_command
open Test_utils

let check_result expected generated =
  Alcotest.(check (result (list event) error) "succeeds" expected generated)
;;

module Data = struct
  module Raw = struct
    let start1 =
      Ptime.of_date_time ((2022, 8, 2), ((10, 57, 5), 2))
      |> CCOption.get_exn_or "Invalid start1"
    ;;

    let start2 =
      Ptime.add_span start1 @@ Ptime.Span.of_int_s 86400
      |> CCOption.get_exn_or "Invalid start2"
    ;;

    let start3 =
      Ptime.add_span start2 @@ Ptime.Span.of_int_s 86400
      |> CCOption.get_exn_or "Invalid start3"
    ;;

    let duration = Ptime.Span.of_int_s 3600
    let description = "Description"
    let limitations = "Limitations"
    let max_participants = 24
    let min_participants = 5
    let overbook = 0
    let lead_time = Ptime.Span.of_int_s 1800

    let sent_at =
      Ptime.add_span start1 @@ Ptime.Span.of_int_s (2 * 86400)
      |> CCOption.get_exn_or "Invalid sent_at"
    ;;

    let assignment_count = 18
  end

  module String = struct
    let start1 = Raw.start1 |> Ptime.to_rfc3339 ~frac_s:12
    let start2 = Raw.start2 |> Ptime.to_rfc3339 ~frac_s:12
    let start3 = Raw.start3 |> Ptime.to_rfc3339 ~frac_s:12
    let duration = Raw.duration |> Pool_common.Utils.Time.timespan_to_minutes
    let description = Raw.description
    let limitations = Raw.limitations
    let max_participants = Raw.max_participants |> string_of_int
    let min_participants = Raw.min_participants |> string_of_int
    let overbook = Raw.overbook |> string_of_int
    let lead_time = Raw.lead_time |> Pool_common.Utils.Time.timespan_to_minutes
    let sent_at = Raw.sent_at |> Ptime.to_rfc3339 ~frac_s:12
    let assignment_count = Raw.assignment_count |> string_of_int
  end

  module Validated = struct
    let start1 = Session.Start.create Raw.start1
    let start2 = Session.Start.create Raw.start2
    let start3 = Session.Start.create Raw.start3
    let duration = Session.Duration.create Raw.duration |> CCResult.get_exn

    let description =
      Session.Description.create Raw.description |> CCResult.get_exn
    ;;

    let limitations =
      Session.Limitations.create Raw.limitations |> CCResult.get_exn
    ;;

    let max_participants =
      Session.ParticipantAmount.create Raw.max_participants |> CCResult.get_exn
    ;;

    let max_participants2 =
      Session.ParticipantAmount.create 5 |> CCResult.get_exn
    ;;

    let min_participants =
      Session.ParticipantAmount.create Raw.min_participants |> CCResult.get_exn
    ;;

    let overbook =
      Session.ParticipantAmount.create Raw.overbook |> CCResult.get_exn
    ;;

    let lead_time =
      Pool_common.Reminder.LeadTime.create Raw.lead_time |> CCResult.get_exn
    ;;

    let sent_at = Pool_common.Reminder.SentAt.create Raw.sent_at

    let assignment_count =
      Session.AssignmentCount.create Raw.assignment_count |> CCResult.get_exn
    ;;
  end

  module Invalid = struct
    let ( start
        , duration
        , description
        , max
        , min
        , overbook
        , subject
        , text
        , lead_time )
      =
      "01", "long", "", "many", "few", "none", "", "", "-1.5"
    ;;
  end

  let input =
    let open Pool_common.Message.Field in
    [ show Start, [ String.start1 ]
    ; show Duration, [ String.duration ]
    ; show Description, [ String.description ]
    ; show Limitations, [ String.limitations ]
    ; show MaxParticipants, [ String.max_participants ]
    ; show MinParticipants, [ String.min_participants ]
    ; show Overbook, [ String.overbook ]
    ; show LeadTime, [ String.lead_time ]
    ; show SentAt, [ String.sent_at ]
    ; show AssignmentCount, [ String.assignment_count ]
    ]
  ;;

  let invalid_input =
    let open Pool_common.Message.Field in
    let open Invalid in
    [ show Start, [ start ]
    ; show Duration, [ duration ]
    ; show Description, [ description ]
    ; show MaxParticipants, [ max ]
    ; show MinParticipants, [ min ]
    ; show Overbook, [ overbook ]
    ; show LeadTime, [ lead_time ]
    ]
  ;;

  let update_input_helper kvs =
    let updater k v =
      CCList.Assoc.update
        ~eq:CCString.equal
        ~f:(function
          | None -> failwith "Key not found"
          | Some _ -> v)
        (Pool_common.Message.Field.show k)
    in
    CCList.fold_left (fun acc (k, v) -> updater k v acc) input kvs
  ;;

  let update_input kvs =
    kvs |> CCList.map (fun (k, v) -> k, Some [ v ]) |> update_input_helper
  ;;

  let delete_from_input ks =
    ks |> CCList.map (fun k -> k, None) |> update_input_helper
  ;;
end

let create_empty_data () =
  let open CCResult.Infix in
  let input = [] in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle experiment_id location)
  in
  check_result
    (Error
       (let open Pool_common.Message in
        let open Field in
        Conformist
          [ Start, NoValue
          ; Duration, NoValue
          ; MaxParticipants, NoValue
          ; MinParticipants, NoValue
          ; Overbook, NoValue
          ]))
    res
;;

let create_invalid_data () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let open Field in
  let open Data.Invalid in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(
      Data.invalid_input |> decode >>= handle experiment_id location)
  in
  check_result
    (Error
       (Conformist
          [ Start, NotADatetime (start, "1: unexpected end of input")
          ; Duration, Invalid Duration
          ; Description, NoValue
          ; MaxParticipants, NotANumber max
          ; MinParticipants, NotANumber min
          ; Overbook, NotANumber overbook
          ; LeadTime, NegativeAmount
          ]))
    res
;;

let create_min_gt_max () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let open Field in
  let input =
    let open Data in
    let open Pool_common.Message.Field in
    update_input [ MaxParticipants, "5"; MinParticipants, "6" ]
  in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle experiment_id location)
  in
  check_result (Error (Smaller (MaxParticipants, MinParticipants))) res
;;

let create_no_optional () =
  let open CCResult.Infix in
  let open Pool_common.Message.Field in
  let session_id = Session.Id.create () in
  let input =
    let open Data in
    delete_from_input
      [ Description; Limitations; LeadTime; SentAt; AssignmentCount ]
  in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(
      input |> decode >>= handle ~session_id experiment_id location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      start1
      duration
      None
      None
      location
      max_participants
      min_participants
      overbook
      None
  in
  check_result
    (Ok [ Pool_event.Session (Session.Created (session, experiment_id)) ])
    res
;;

let create_full () =
  let open CCResult.Infix in
  let experiment_id = Experiment.Id.create () in
  let session_id = Session.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(
      Data.input |> decode >>= handle ~session_id experiment_id location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      start1
      duration
      (Some description)
      (Some limitations)
      location
      max_participants
      min_participants
      overbook
      (Some lead_time)
  in
  check_result
    (Ok [ Pool_event.Session (Session.Created (session, experiment_id)) ])
    res
;;

let create_min_eq_max () =
  let open CCResult.Infix in
  let session_id = Session.Id.create () in
  let input =
    let open Data in
    let open Pool_common.Message.Field in
    update_input [ MaxParticipants, "5"; MinParticipants, "5" ]
  in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(
      input |> decode >>= handle ~session_id experiment_id location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      start1
      duration
      (Some description)
      (Some limitations)
      location
      max_participants2
      min_participants
      overbook
      (Some lead_time)
  in
  check_result
    (Ok [ Pool_event.Session (Session.Created (session, experiment_id)) ])
    res
;;

let update_empty_data () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Model.create_session () in
  let input = [] in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result
    (Error
       (let open Pool_common.Message in
        let open Field in
        Conformist
          [ MaxParticipants, NoValue
          ; MinParticipants, NoValue
          ; Overbook, NoValue
          ]))
    res
;;

(* TODO [aerben] test updating empty start & desc with has_assignments *)

let update_invalid_data () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let open Field in
  let open Data.Invalid in
  let location = Location_test.create_location () in
  let session = Model.create_session () in
  let res =
    SessionC.Update.(
      Data.invalid_input |> decode >>= handle [] session location)
  in
  check_result
    (Error
       (Conformist
          [ Start, NotADatetime (start, "1: unexpected end of input")
          ; Duration, Invalid Duration
          ; Description, NoValue
          ; MaxParticipants, NotANumber max
          ; MinParticipants, NotANumber min
          ; Overbook, NotANumber overbook
          ; LeadTime, NegativeAmount
          ]))
    res
;;

let update_min_gt_max () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let open Field in
  let input =
    let open Data in
    let open Pool_common.Message.Field in
    update_input [ MaxParticipants, "5"; MinParticipants, "6" ]
  in
  let session = Model.create_session () in
  let location = Location_test.create_location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result (Error (Smaller (MaxParticipants, MinParticipants))) res
;;

let update_no_optional () =
  let open CCResult.Infix in
  let open Pool_common.Message.Field in
  let input =
    let open Data in
    delete_from_input
      [ Description; Limitations; LeadTime; SentAt; AssignmentCount ]
  in
  let session = Model.create_session () in
  let location = Location_test.create_location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = start1
                 ; duration
                 ; description = None
                 ; limitations = None
                 ; max_participants
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = None
                 }
               , location
               , session )))
       ])
    res
;;

let update_full () =
  let open CCResult.Infix in
  let session = Model.create_session () in
  let location = Location_test.create_location () in
  let input =
    let open Data in
    update_input [ Pool_common.Message.Field.Start, String.start2 ]
  in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = start2
                 ; duration
                 ; description = Some description
                 ; limitations = Some limitations
                 ; max_participants
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = Some lead_time
                 }
               , location
               , session )))
       ])
    res
;;

let update_min_eq_max () =
  let open CCResult.Infix in
  let input =
    let open Data in
    let open Pool_common.Message.Field in
    update_input [ MaxParticipants, "5"; MinParticipants, "5" ]
  in
  let session = Model.create_session () in
  let location = Location_test.create_location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = start1
                 ; duration
                 ; description = Some description
                 ; limitations = Some limitations
                 ; max_participants = max_participants2
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = Some lead_time
                 }
               , location
               , session )))
       ])
    res
;;

let delete () =
  let session = Model.create_session () in
  let res =
    SessionC.Delete.(handle { session; follow_ups = []; templates = [] })
  in
  check_result (Ok [ Pool_event.Session (Session.Deleted session) ]) res
;;

let delete_closed_session () =
  let closed_at = Ptime_clock.now () in
  let session = Model.create_session () in
  let session = Session.{ session with closed_at = Some closed_at } in
  let res =
    SessionC.Delete.(handle { session; follow_ups = []; templates = [] })
  in
  check_result
    (closed_at
     |> Pool_common.Utils.Time.formatted_date_time
     |> Pool_common.Message.sessionalreadyclosed
     |> CCResult.fail)
    res
;;

let delete_session_with_assignments () =
  let session = Model.create_session () in
  let session =
    Session.
      { session with
        assignment_count = AssignmentCount.create 1 |> CCResult.get_exn
      }
  in
  let res =
    SessionC.Delete.(handle { session; follow_ups = []; templates = [] })
  in
  check_result (Error Pool_common.Message.SessionHasAssignments) res
;;

let delete_session_with_follow_ups () =
  let id = Session.Id.create () in
  let session = Model.create_session ~id () in
  let follow_up_session = Model.create_session ~follow_up_to:id () in
  let res =
    SessionC.Delete.(
      handle { session; follow_ups = [ follow_up_session ]; templates = [] })
  in
  check_result (Error Pool_common.Message.SessionHasFollowUps) res
;;

let create_cancellation_message reason contact =
  let recipient =
    contact |> Contact.email_address |> Pool_user.EmailAddress.value
  in
  let email = Model.create_email ~recipient () in
  reason
  |> Session.CancellationReason.value
  |> CCFun.flip Sihl_email.set_text email
  |> CCResult.return
;;

let create_cancellation_text_message
  (_ : Session.CancellationReason.t)
  (_ : Contact.t)
  cell_phone
  =
  Model.create_text_message cell_phone |> CCResult.return
;;

let cancel_no_reason () =
  let open CCResult.Infix in
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "" ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result
    (Error
       (let open Pool_common.Message in
        Conformist [ Field.Reason, NoValue ]))
    res
;;

let cancel_no_message_channels () =
  let open CCResult.Infix in
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [])
  in
  check_result
    (Error Pool_common.Message.(NoOptionSelected Field.NotifyVia))
    res
;;

let cancel_in_past () =
  let open CCResult.Infix in
  let twohours = Ptime.Span.of_int_s @@ (120 * 60) in
  let session =
    Session.
      { (Model.create_session ()) with
        start =
          Ptime.sub_span (Ptime_clock.now ()) twohours
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      }
  in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ]; "notify_via", [ "email" ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result (Error Pool_common.Message.SessionInPast) res
;;

let cancel_already_canceled () =
  let open CCResult.Infix in
  let now = Ptime_clock.now () in
  let session =
    Session.{ (Model.create_session ()) with canceled_at = Some now }
  in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ]; "notify_via", [ "email" ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result
    (now
     |> Pool_common.Utils.Time.formatted_date_time
     |> Pool_common.Message.sessionalreadycanceled
     |> CCResult.fail)
    res
;;

let cancel_valid () =
  let open CCResult.Infix in
  let session1 = Model.create_session () in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let contact_events =
    assignments
    |> CCList.map (fun (contact, assignments) ->
      Contact_counter.update_on_session_cancellation assignments contact
      |> Contact.updated
      |> Pool_event.contact)
  in
  let reason = "Experimenter is ill" in
  let res =
    SessionC.Cancel.(
      [ "reason", [ reason ] ]
      |> decode
      >>= handle
            [ session1 ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  let messages =
    assignments
    |> CCList.map (fun (contact, _) ->
      create_cancellation_message
        (reason |> Session.CancellationReason.of_string)
        contact
      |> Test_utils.get_or_failwith_pool_error
      |> fun msg ->
      (msg, experiment.Experiment.smtp_auth_id)
      |> Email.sent
      |> Pool_event.email)
  in
  check_result
    (Ok
       (messages
        @ [ Pool_event.Session (Session.Canceled session1) ]
        @ contact_events))
    res;
  let halfhour = Ptime.Span.of_int_s @@ (30 * 60) in
  let session2 =
    Session.
      { session1 with
        start =
          (* Can still cancel ongoing session *)
          Ptime.sub_span (Ptime_clock.now ()) halfhour
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      }
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ reason ] ]
      |> decode
      >>= handle
            [ session2 ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.TextMessage ])
  in
  let text_messags =
    let reason = Session.CancellationReason.of_string "reason" in
    assignments
    |> CCList.map (fun (contact, _) ->
      contact.Contact.cell_phone
      |> CCOption.get_exn_or "No phone number provided"
      |> create_cancellation_text_message reason contact
      |> get_or_failwith_pool_error
      |> Text_message.sent
      |> Pool_event.text_message)
  in
  check_result
    (Ok
       (text_messags
        @ (Pool_event.Session (Session.Canceled session2) :: contact_events)))
    res
;;

let cancel_valid_with_missing_cell_phone () =
  let open CCResult.Infix in
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let contact1 = Model.create_contact () in
  let contact2 = Contact.{ (Model.create_contact ()) with cell_phone = None } in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let contact_events =
    assignments
    |> CCList.map (fun (contact, assignments) ->
      Contact_counter.update_on_session_cancellation assignments contact
      |> Contact.updated
      |> Pool_event.contact)
  in
  let reason = "Experimenter is ill" in
  let messages =
    let text_msg =
      contact1.Contact.cell_phone
      |> CCOption.to_result Pool_common.Message.(Invalid Field.CellPhone)
      |> Test_utils.get_or_failwith_pool_error
      |> create_cancellation_text_message
           (Session.CancellationReason.of_string reason)
           contact1
      |> Test_utils.get_or_failwith_pool_error
      |> Text_message.sent
      |> Pool_event.text_message
    in
    let email =
      create_cancellation_message
        (reason |> Session.CancellationReason.of_string)
        contact2
      |> Test_utils.get_or_failwith_pool_error
      |> fun msg ->
      (msg, experiment.Experiment.smtp_auth_id)
      |> Email.sent
      |> Pool_event.email
    in
    [ text_msg; email ]
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ reason ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            [ Pool_common.NotifyVia.TextMessage ])
  in
  check_result
    (Ok
       (messages
        @ [ Session.Canceled session |> Pool_event.session ]
        @ contact_events))
    res
;;

let cancel_with_email_and_text_notification () =
  let open CCResult.Infix in
  let session = Model.create_session () in
  let contact1 = Model.create_contact () in
  let experiment = Model.create_experiment () in
  let contact2 = Contact.{ (Model.create_contact ()) with cell_phone = None } in
  let contacts = [ contact1; contact2 ] in
  let assignments =
    CCList.map (fun contact -> Model.create_assignment ~contact ()) contacts
    |> Assignment.group_by_contact
  in
  let contact_events =
    assignments
    |> CCList.map (fun (contact, assignments) ->
      Contact_counter.update_on_session_cancellation assignments contact
      |> Contact.updated
      |> Pool_event.contact)
  in
  let reason = "Experimenter is ill" in
  let messages =
    let email contact =
      create_cancellation_message
        (reason |> Session.CancellationReason.of_string)
        contact
      |> Test_utils.get_or_failwith_pool_error
      |> fun msg ->
      (msg, experiment.Experiment.smtp_auth_id)
      |> Email.sent
      |> Pool_event.email
    in
    let text_msg contact =
      contact.Contact.cell_phone
      |> CCOption.to_result Pool_common.Message.(Invalid Field.CellPhone)
      |> Test_utils.get_or_failwith_pool_error
      |> create_cancellation_text_message
           (Session.CancellationReason.of_string reason)
           contact
      |> Test_utils.get_or_failwith_pool_error
      |> Text_message.sent
      |> Pool_event.text_message
    in
    let email_events = contacts |> CCList.map email in
    let text_msg_event = contact1 |> text_msg in
    text_msg_event :: email_events
  in
  let res =
    SessionC.Cancel.(
      [ "reason", [ reason ] ]
      |> decode
      >>= handle
            [ session ]
            experiment
            assignments
            create_cancellation_message
            create_cancellation_text_message
            Pool_common.NotifyVia.[ TextMessage; Email ])
  in
  check_result
    (Ok
       (messages
        @ [ Session.Canceled session |> Pool_event.session ]
        @ contact_events))
    res
;;

let close_before_start () =
  let session = Test_utils.Model.(create_session ~start:(in_an_hour ())) () in
  let res =
    Cqrs_command.Assignment_command.SetAttendance.handle session [] []
  in
  check_result (Error Pool_common.Message.SessionNotStarted) res
;;

let close_valid () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let res = handle session [] [] in
  check_result (Ok [ Session.Closed session |> Pool_event.session ]) res
;;

let close_valid_with_assignments () =
  let open Cqrs_command.Assignment_command in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let assignments =
    [ true ]
    |> CCList.map (fun participated ->
      ()
      |> Test_utils.Model.create_contact
      |> create
      |> fun assignment ->
      ( assignment
      , NoShow.create false
      , Participated.create participated
      , Assignment.IncrementParticipationCount.create true
      , None ))
  in
  let tags = Tag_test.Data.Tag.create_with_description () |> CCList.return in
  let res = SetAttendance.handle session tags assignments in
  let expected =
    CCList.fold_left
      (fun events
           ( (assignment : Assignment.t)
           , no_show
           , participated
           , _
           , (_ : t list option) ) ->
        let contact_event =
          let open Contact in
          let contact =
            assignment.contact
            |> update_num_show_ups ~step:1
            |> update_num_participations ~step:1
          in
          Updated contact |> Pool_event.contact
        in
        let tag_events =
          let open Tags in
          tags
          |> CCList.map (fun (tag : t) ->
            Tagged
              { Tagged.model_uuid = Contact.id assignment.contact
              ; tag_uuid = tag.id
              }
            |> Pool_event.tags)
        in
        events
        @ [ AttendanceSet (assignment, no_show, participated)
            |> Pool_event.assignment
          ; contact_event
          ]
        @ tag_events)
      [ Session.Closed session |> Pool_event.session ]
      assignments
    |> CCResult.return
  in
  check_result expected res
;;

let close_with_deleted_assignment () =
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let command =
    let open Assignment in
    let base = Test_utils.Model.create_assignment () in
    let assignment =
      { base with marked_as_deleted = MarkedAsDeleted.create true }
    in
    let no_show = NoShow.create false in
    let participated = Participated.create true in
    ( assignment
    , no_show
    , participated
    , Assignment.IncrementParticipationCount.create false
    , None )
  in
  let res =
    Cqrs_command.Assignment_command.SetAttendance.handle session [] [ command ]
  in
  check_result
    (Error Pool_common.Message.(IsMarkedAsDeleted Field.Assignment))
    res
;;

let validate_invalid_participation () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let participation =
    ( Test_utils.Model.create_contact () |> create
    , NoShow.create true
    , Participated.create true
    , Assignment.IncrementParticipationCount.create false
    , None )
  in
  let res = handle session [] [ participation ] in
  let expected =
    Error Pool_common.Message.(MutuallyExclusive Field.(Participated, NoShow))
  in
  check_result expected res
;;

let close_unparticipated_with_followup () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let open Test_utils in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let contact = Model.create_contact () in
  let assignment = Model.create_assignment ~contact () in
  let follow_up = Model.create_assignment ~contact () in
  let participation =
    ( assignment
    , NoShow.create false
    , Participated.create false
    , Assignment.IncrementParticipationCount.create true
    , Some [ follow_up ] )
  in
  let res = handle session [] [ participation ] in
  let expected =
    let contact =
      let open Contact in
      contact
      |> update_num_show_ups ~step:1
      |> update_num_participations ~step:1
    in
    Ok
      [ Session.Closed session |> Pool_event.session
      ; Assignment.AttendanceSet
          (assignment, NoShow.create false, Participated.create false)
        |> Pool_event.assignment
      ; Contact.Updated contact |> Pool_event.contact
      ; Assignment.MarkedAsDeleted follow_up |> Pool_event.assignment
      ]
  in
  check_result expected res
;;

let send_reminder () =
  let session1 = Test_utils.Model.create_session () in
  let session2 =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start1
    }
  in
  let experiment = Model.create_experiment () in
  let users =
    CCList.range 1 4
    |> CCList.map (fun i ->
      Sihl_email.create
        ~sender:"admin@mail.com"
        ~recipient:(CCFormat.asprintf "user%i@mail.com" i)
        ~subject:"Reminder"
        "Hello, this is a reminder for the session")
  in
  let res =
    SessionC.SendReminder.handle
      [ session1, experiment, CCList.take 2 users
      ; session2, experiment, CCList.drop 2 users
      ]
  in
  let smtp_auth_id = experiment.Experiment.smtp_auth_id in
  check_result
    (Ok
       (CCList.flat_map
          (fun (s, es) ->
            let es = es |> CCList.map (fun email -> email, smtp_auth_id) in
            [ Pool_event.session (Session.ReminderSent s)
            ; Pool_event.email (Email.BulkSent es)
            ])
          [ session1, CCList.take 2 users; session2, CCList.drop 2 users ]))
    res
;;

let create_follow_up_earlier () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let session = Test_utils.Model.create_session () in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(
      Data.input
      |> decode
      >>= handle ~parent_session:session experiment_id location)
  in
  check_result (Error FollowUpIsEarlierThanMain) res
;;

let create_follow_up_later () =
  let open CCResult.Infix in
  let parent_session = Test_utils.Model.create_session () in
  let session_id = Session.Id.create () in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let later_start =
    parent_session.Session.start
    |> Session.Start.value
    |> CCFun.flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    let open Data in
    update_input
      [ Pool_common.Message.Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start
      ]
  in
  let res =
    SessionC.Create.(
      input
      |> decode
      >>= handle ~session_id ~parent_session experiment_id location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      ~follow_up_to:parent_session.Session.id
      (Session.Start.create later_start)
      duration
      (Some description)
      (Some limitations)
      location
      max_participants
      min_participants
      overbook
      (Some lead_time)
  in
  check_result
    (Ok [ Pool_event.Session (Session.Created (session, experiment_id)) ])
    res
;;

let update_follow_up_earlier () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let session = Test_utils.Model.create_session () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Update.(
      Data.input
      |> decode
      >>= handle ~parent_session:session [] session location)
  in
  check_result (Error FollowUpIsEarlierThanMain) res
;;

let update_follow_up_later () =
  let open CCResult.Infix in
  let session = Test_utils.Model.create_session () in
  let location = Location_test.create_location () in
  let later_start =
    session.Session.start
    |> Session.Start.value
    |> CCFun.flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    let open Data in
    update_input
      [ Pool_common.Message.Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start
      ]
  in
  let res =
    SessionC.Update.(
      input |> decode >>= handle ~parent_session:session [] session location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = Session.Start.create later_start
                 ; duration
                 ; description = Some description
                 ; limitations = Some limitations
                 ; max_participants
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = Some lead_time
                 }
               , location
               , session )))
       ])
    res
;;

let update_follow_ups_earlier () =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let location = Location_test.create_location () in
  (* Valid starting setup, main session happens before two follow-ups *)
  let session =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start1
    }
  in
  let follow_up1 =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start2
    }
  in
  let follow_up2 =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start3
    }
  in
  (* Make input start after first follow-up *)
  let later_start1 =
    Data.Validated.start2
    |> Session.Start.value
    |> CCFun.flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
    |> Ptime.to_rfc3339 ~frac_s:12
  in
  let input1 =
    let open Data in
    update_input [ Pool_common.Message.Field.Start, later_start1 ]
  in
  let res_earlier_one =
    SessionC.Update.(
      input1 |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result (Error FollowUpIsEarlierThanMain) res_earlier_one;
  (* Make input start after both follow-ups *)
  let later_start2 =
    Data.Validated.start3
    |> Session.Start.value
    |> CCFun.flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
    |> Ptime.to_rfc3339 ~frac_s:12
  in
  let input2 =
    let open Data in
    update_input [ Pool_common.Message.Field.Start, later_start2 ]
  in
  let res_earlier_all =
    SessionC.Update.(
      input2 |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result (Error FollowUpIsEarlierThanMain) res_earlier_all
;;

let update_follow_ups_later () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  (* Valid starting setup, main session happens before two follow-ups *)
  let session =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start1
    }
  in
  let follow_up1 =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start2
    }
  in
  let follow_up2 =
    { (Test_utils.Model.create_session ()) with
      Session.start = Data.Validated.start3
    }
  in
  let res_normal =
    SessionC.Update.(
      Data.input
      |> decode
      >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = start1
                 ; duration
                 ; description = Some description
                 ; limitations = Some limitations
                 ; max_participants
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = Some lead_time
                 }
               , location
               , session )))
       ])
    res_normal;
  (* Make input start later, but before both follow-ups *)
  let later_start =
    Data.Validated.start1
    |> Session.Start.value
    |> CCFun.flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    let open Data in
    update_input
      [ Pool_common.Message.Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start
      ]
  in
  let res_later_but_earlier =
    SessionC.Update.(
      input |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Updated
              (let open Data.Validated in
               ( { Session.start = Session.Start.create later_start
                 ; duration
                 ; description = Some description
                 ; limitations = Some limitations
                 ; max_participants
                 ; min_participants
                 ; overbook
                 ; reminder_lead_time = Some lead_time
                 }
               , location
               , session )))
       ])
    res_later_but_earlier
;;

let reschedule_to_past () =
  let session = Test_utils.Model.create_session () in
  let experiment = Model.create_experiment () in
  let create_message _ _ _ =
    Test_utils.Model.create_email () |> CCResult.return
  in
  let command =
    Session.
      { start =
          Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s @@ (60 * 60))
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; duration = session.Session.duration
      }
  in
  let events =
    SessionC.Reschedule.handle [] session [] experiment create_message command
  in
  let expected = Error Pool_common.Message.TimeInPast in
  Test_utils.check_result expected events
;;

let reschedule_with_experiment_smtp () =
  let session = Test_utils.Model.create_session () in
  let experiment = Model.create_experiment () in
  let smtp_auth_id = Email.SmtpAuth.Id.create () in
  let experiment =
    Experiment.{ experiment with smtp_auth_id = Some smtp_auth_id }
  in
  let assignment = Test_utils.Model.create_assignment () in
  let create_message _ _ _ =
    Test_utils.Model.create_email () |> CCResult.return
  in
  let command =
    Session.
      { start = Test_utils.Model.in_two_hours ()
      ; duration = session.Session.duration
      }
  in
  let events =
    SessionC.Reschedule.handle
      []
      session
      [ assignment ]
      experiment
      create_message
      command
  in
  let expected =
    Ok
      [ Session.Rescheduled (session, command) |> Pool_event.session
      ; Email.BulkSent [ Test_utils.Model.create_email (), Some smtp_auth_id ]
        |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;

let close_session_check_contact_figures _ () =
  let open Utils.Lwt_result.Infix in
  let open Integration_utils in
  let open Test_utils in
  let%lwt experiment = Repo.first_experiment () in
  let%lwt session =
    SessionRepo.create ~start:(Model.an_hour_ago ()) experiment.Experiment.id ()
  in
  let%lwt participated_c = ContactRepo.create ~with_terms_accepted:true () in
  let%lwt show_up_c = ContactRepo.create ~with_terms_accepted:true () in
  let%lwt no_show_c = ContactRepo.create ~with_terms_accepted:true () in
  let contacts =
    [ participated_c, `Participated; show_up_c, `ShowUp; no_show_c, `NoShow ]
  in
  let%lwt () =
    contacts |> Lwt_list.iter_s CCFun.(fst %> AssignmentRepo.create session)
  in
  let%lwt assignments =
    Assignment.find_by_session Data.database_label session.Session.id
    ||> get_or_failwith_pool_error
  in
  let find_assignment contact =
    CCList.find
      Contact.(
        fun (assignment : Assignment.t) ->
          Id.equal (id assignment.Assignment.contact) (id contact))
      assignments
  in
  let%lwt () =
    let open CCList in
    contacts
    |> map (fun (contact, status) ->
      let open Assignment in
      let open Contact in
      let no_show, participated, increment_num_participatons =
        match status with
        | `Participated ->
          ( NoShow.create false
          , Participated.create true
          , IncrementParticipationCount.create true )
        | `ShowUp ->
          ( NoShow.create false
          , Participated.create false
          , IncrementParticipationCount.create false )
        | `NoShow ->
          ( NoShow.create true
          , Participated.create false
          , IncrementParticipationCount.create false )
      in
      let contact =
        Contact_counter.update_on_session_closing
          contact
          no_show
          participated
          increment_num_participatons
        |> Test_utils.get_or_failwith_pool_error
      in
      [ AttendanceSet (find_assignment contact, no_show, participated)
        |> Pool_event.assignment
      ; Updated contact |> Pool_event.contact
      ])
    |> flatten
    |> cons (Session.Closed session |> Pool_event.session)
    |> Pool_event.handle_events Data.database_label
  in
  let%lwt res =
    contacts
    |> Lwt_list.map_s (fun (contact, status) ->
      let open Contact in
      let num_show_ups, num_no_shows, num_participations =
        (match status with
         | `Participated -> 1, 0, 1
         | `ShowUp -> 1, 0, 0
         | `NoShow -> 0, 1, 0)
        |> fun (show_up, no_show, participation) ->
        ( NumberOfShowUps.of_int show_up
        , NumberOfNoShows.of_int no_show
        , NumberOfParticipations.of_int participation )
      in
      let%lwt contact =
        find_by_email Data.database_label (Contact.email_address contact)
        ||> get_or_failwith_pool_error
      in
      (NumberOfShowUps.equal contact.num_show_ups num_show_ups
       && NumberOfNoShows.equal contact.num_no_shows num_no_shows
       && NumberOfParticipations.equal
            contact.num_participations
            num_participations)
      |> Lwt.return)
    ||> CCList.filter not
    ||> CCList.is_empty
  in
  let () = Alcotest.(check bool "succeeds" true res) in
  Lwt.return_unit
;;
