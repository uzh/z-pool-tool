module SessionC = Cqrs_command.Session_command

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
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
    let duration = Raw.duration |> Pool_common.Utils.Time.timespan_to_hours
    let description = Raw.description
    let max_participants = Raw.max_participants |> string_of_int
    let min_participants = Raw.min_participants |> string_of_int
    let overbook = Raw.overbook |> string_of_int
    let lead_time = Raw.lead_time |> Pool_common.Utils.Time.timespan_to_hours
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
        ~f:
          (function
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

let[@warning "-21"] create_min_gt_max () =
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
  let input =
    let open Data in
    delete_from_input [ Description; LeadTime; SentAt; AssignmentCount ]
  in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle experiment_id location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Created
              (let open Data.Validated in
              ( { Session.start = start1
                ; duration
                ; description = None
                ; max_participants
                ; min_participants
                ; overbook
                ; reminder_lead_time = None
                }
              , None
              , experiment_id
              , location )))
       ])
    res
;;

let create_full () =
  let open CCResult.Infix in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(Data.input |> decode >>= handle experiment_id location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Created
              (let open Data.Validated in
              ( { Session.start = start1
                ; duration
                ; description = Some description
                ; max_participants
                ; min_participants
                ; overbook
                ; reminder_lead_time = Some lead_time
                }
              , None
              , experiment_id
              , location )))
       ])
    res
;;

let create_min_eq_max () =
  let open CCResult.Infix in
  let input =
    let open Data in
    let open Pool_common.Message.Field in
    update_input [ MaxParticipants, "5"; MinParticipants, "5" ]
  in
  let experiment_id = Experiment.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle experiment_id location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Created
              (let open Data.Validated in
              ( { Session.start = start1
                ; duration
                ; description = Some description
                ; max_participants = max_participants2
                ; min_participants
                ; overbook
                ; reminder_lead_time = Some lead_time
                }
              , None
              , experiment_id
              , location )))
       ])
    res
;;

let update_empty_data () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Test_utils.Model.create_session () in
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
  let session = Test_utils.Model.create_session () in
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
  let session = Test_utils.Model.create_session () in
  let location = Location_test.create_location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result (Error (Smaller (MaxParticipants, MinParticipants))) res
;;

let update_no_optional () =
  let open CCResult.Infix in
  let open Pool_common.Message.Field in
  let input =
    let open Data in
    delete_from_input [ Description; LeadTime; SentAt; AssignmentCount ]
  in
  let session = Test_utils.Model.create_session () in
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
  let session = Test_utils.Model.create_session () in
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
  let session = Test_utils.Model.create_session () in
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
  let session = Test_utils.Model.create_session () in
  let res = SessionC.Delete.handle session in
  check_result (Ok [ Pool_event.Session (Session.Deleted session) ]) res
;;

let delete_closed_session () =
  let closed_at = Ptime_clock.now () in
  let session = Test_utils.Model.create_session () in
  let session = Session.{ session with closed_at = Some closed_at } in
  let res = SessionC.Delete.handle session in
  check_result
    (closed_at
    |> Pool_common.Utils.Time.formatted_date_time
    |> Pool_common.Message.sessionalreadyclosed
    |> CCResult.fail)
    res
;;

let delete_session_with_assignments () =
  let session = Test_utils.Model.create_session () in
  let session =
    Session.
      { session with
        assignment_count = AssignmentCount.create 1 |> CCResult.get_exn
      }
  in
  let res = SessionC.Delete.handle session in
  check_result (Error Pool_common.Message.SessionHasAssignments) res
;;

let create_cancellation_message reason contact =
  let recipient =
    contact |> Contact.email_address |> Pool_user.EmailAddress.value
  in
  let email = Test_utils.Model.create_email ~recipient () in
  reason
  |> Session.CancellationReason.value
  |> CCFun.flip Sihl_email.set_text email
;;

let cancel_no_reason () =
  let open CCResult.Infix in
  let session = Test_utils.Model.create_session () in
  let contact1 = Test_utils.Model.create_contact () in
  let contact2 = Test_utils.Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "" ]; "email", [ "true" ]; "sms", [ "true" ] ]
      |> decode
      >>= handle session contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
  in
  check_result
    (Error
       (let open Pool_common.Message in
       Conformist [ Field.Reason, NoValue ]))
    res
;;

let cancel_no_message_channels () =
  let open CCResult.Infix in
  let session = Test_utils.Model.create_session () in
  let contact1 = Test_utils.Model.create_contact () in
  let contact2 = Test_utils.Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ]
      ; "email", [ "false" ]
      ; "sms", [ "false" ]
      ]
      |> decode
      >>= handle session contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
  in
  check_result (Error Pool_common.Message.PickMessageChannel) res
;;

let cancel_in_past () =
  let open CCResult.Infix in
  let twohours = Ptime.Span.of_int_s @@ (120 * 60) in
  let session =
    Session.
      { (Test_utils.Model.create_session ()) with
        start =
          Ptime.sub_span (Ptime_clock.now ()) twohours
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      }
  in
  let contact1 = Test_utils.Model.create_contact () in
  let contact2 = Test_utils.Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ]
      ; "email", [ "true" ]
      ; "sms", [ "false" ]
      ]
      |> decode
      >>= handle session contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
  in
  check_result (Error Pool_common.Message.SessionInPast) res
;;

let cancel_already_canceled () =
  let open CCResult.Infix in
  let now = Ptime_clock.now () in
  let session =
    Session.{ (Test_utils.Model.create_session ()) with canceled_at = Some now }
  in
  let contact1 = Test_utils.Model.create_contact () in
  let contact2 = Test_utils.Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let res =
    SessionC.Cancel.(
      [ "reason", [ "Experimenter is ill" ]
      ; "email", [ "true" ]
      ; "sms", [ "false" ]
      ]
      |> decode
      >>= handle session contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
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
  let session1 = Test_utils.Model.create_session () in
  let contact1 = Test_utils.Model.create_contact () in
  let contact2 = Test_utils.Model.create_contact () in
  let contacts = [ contact1; contact2 ] in
  let reason = "Experimenter is ill" in
  let res =
    SessionC.Cancel.(
      [ "reason", [ reason ]; "email", [ "true" ]; "sms", [ "true" ] ]
      |> decode
      >>= handle session1 contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
  in
  let messages =
    contacts
    |> CCList.map
         (create_cancellation_message
            (reason |> Session.CancellationReason.of_string))
  in
  check_result
    (Ok
       (* TODO issue #149 extend test with sms events *)
       [ Pool_event.Email (Email.BulkSent messages)
       ; Pool_event.Session (Session.Canceled session1)
       ])
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
      [ "reason", [ reason ]; "email", [ "false" ]; "sms", [ "true" ] ]
      |> decode
      >>= handle session2 contacts (fun r c ->
            create_cancellation_message r c |> CCResult.pure))
  in
  check_result
    (Ok
       (* TODO issue #149 extend test with sms events *)
       [ Pool_event.Session (Session.Canceled session2) ])
    res
;;

let close_before_start () =
  let session = Test_utils.Model.(create_session ~start:(in_an_hour ())) () in
  let res = Cqrs_command.Assignment_command.SetAttendance.handle session [] in
  check_result (Error Pool_common.Message.SessionNotStarted) res
;;

let close_valid () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let res = handle session [] in
  check_result (Ok [ Session.Closed session |> Pool_event.session ]) res
;;

let close_valid_with_assignments () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let assignments =
    [ true ]
    |> CCList.map (fun participated ->
         ()
         |> Test_utils.Model.create_contact
         |> create
         |> fun assignment ->
         assignment, true |> ShowUp.create, Participated.create participated)
  in
  let res = handle session assignments in
  let expected =
    CCList.fold_left
      (fun events
           (((assignment : Assignment.t), showup, participated) as
           participation) ->
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
        @ [ AttendanceSet participation |> Pool_event.assignment
          ; contact_event
          ])
      [ Session.Closed session |> Pool_event.session ]
      assignments
    |> CCResult.pure
  in
  check_result expected res
;;

let validate_invalid_participation () =
  let open Cqrs_command.Assignment_command.SetAttendance in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let participation =
    ( Test_utils.Model.create_contact () |> create
    , ShowUp.create false
    , Participated.create true )
  in
  let res = handle session [ participation ] in
  let expected =
    Error
      Pool_common.Message.(FieldRequiresCheckbox Field.(Participated, ShowUp))
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
      [ session1, CCList.take 2 users; session2, CCList.drop 2 users ]
  in
  check_result
    (Ok
       (CCList.flat_map
          (fun (s, es) ->
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
  let session = Test_utils.Model.create_session () in
  let experiment_id = Experiment.Id.create () in
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
    SessionC.Create.(
      input |> decode >>= handle ~parent_session:session experiment_id location)
  in
  check_result
    (Ok
       [ Pool_event.Session
           (Session.Created
              (let open Data.Validated in
              ( { Session.start = Session.Start.create later_start
                ; duration
                ; description = Some description
                ; max_participants
                ; min_participants
                ; overbook
                ; reminder_lead_time = Some lead_time
                }
              , Some session.Session.id
              , experiment_id
              , location )))
       ])
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
    SessionC.Reschedule.handle [] session [] create_message command
  in
  let expected = Error Pool_common.Message.TimeInPast in
  Test_utils.check_result expected events
;;
