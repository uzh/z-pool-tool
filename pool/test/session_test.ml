open CCFun
open Test_utils
open Pool_message
module SessionC = Cqrs_command.Session_command
module TimeUnit = Pool_model.Base.TimeUnit

let current_user () = Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin

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
    let duration_unit = TimeUnit.Minutes
    let internal_description = "Description"
    let public_description = "Limitations"
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
    let duration = Raw.duration |> Pool_model.Time.timespan_to_minutes
    let duration_unit = Raw.duration_unit |> TimeUnit.show
    let internal_description = Raw.internal_description
    let public_description = Raw.public_description
    let max_participants = Raw.max_participants |> string_of_int
    let min_participants = Raw.min_participants |> string_of_int
    let overbook = Raw.overbook |> string_of_int
    let lead_time = Raw.lead_time |> Pool_model.Time.timespan_to_minutes
    let sent_at = Raw.sent_at |> Ptime.to_rfc3339 ~frac_s:12
    let assignment_count = Raw.assignment_count |> string_of_int
  end

  module Validated = struct
    let start1 = Session.Start.create Raw.start1
    let start2 = Session.Start.create Raw.start2
    let start3 = Session.Start.create Raw.start3
    let duration = Session.Duration.create Raw.duration |> CCResult.get_exn

    let internal_description =
      Session.InternalDescription.create Raw.internal_description |> CCResult.get_exn
    ;;

    let public_description =
      Session.PublicDescription.create Raw.public_description |> CCResult.get_exn
    ;;

    let max_participants =
      Session.ParticipantAmount.create Raw.max_participants |> CCResult.get_exn
    ;;

    let max_participants2 = Session.ParticipantAmount.create 5 |> CCResult.get_exn

    let min_participants =
      Session.ParticipantAmount.create Raw.min_participants |> CCResult.get_exn
    ;;

    let overbook = Session.ParticipantAmount.create Raw.overbook |> CCResult.get_exn

    let email_reminder_lead_time =
      Pool_common.Reminder.EmailLeadTime.create Raw.lead_time |> CCResult.get_exn
    ;;

    let text_message_reminder_lead_time =
      Pool_common.Reminder.TextMessageLeadTime.create Raw.lead_time |> CCResult.get_exn
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
        , lead_time
        , time_unit )
      =
      "01", "long", "", "many", "few", "none", "", "", "-1.5", "moment"
    ;;
  end

  let input =
    let open Field in
    [ show Start, [ String.start1 ]
    ; show Duration, [ String.duration ]
    ; show (TimeUnitOf Duration), [ String.duration_unit ]
    ; show InternalDescription, [ String.internal_description ]
    ; show PublicDescription, [ String.public_description ]
    ; show MaxParticipants, [ String.max_participants ]
    ; show MinParticipants, [ String.min_participants ]
    ; show Overbook, [ String.overbook ]
    ; show EmailLeadTime, [ String.lead_time ]
    ; show (TimeUnitOf EmailLeadTime), [ String.duration_unit ]
    ; show TextMessageLeadTime, [ String.lead_time ]
    ; show (TimeUnitOf TextMessageLeadTime), [ String.duration_unit ]
    ; show SentAt, [ String.sent_at ]
    ; show AssignmentCount, [ String.assignment_count ]
    ]
  ;;

  let invalid_input =
    let open Field in
    let open Invalid in
    [ show Start, [ start ]
    ; show Duration, [ duration ]
    ; show (TimeUnitOf Duration), [ time_unit ]
    ; show InternalDescription, [ description ]
    ; show PublicDescription, [ description ]
    ; show MaxParticipants, [ max ]
    ; show MinParticipants, [ min ]
    ; show Overbook, [ overbook ]
    ; show EmailLeadTime, [ lead_time ]
    ]
  ;;

  let update_input_helper kvs =
    let updater k v =
      CCList.Assoc.update
        ~eq:CCString.equal
        ~f:(function
          | None -> failwith (Format.asprintf "Key '%s' not found" (Field.show k))
          | Some _ -> v)
        (Field.show k)
    in
    CCList.fold_left (fun acc (k, v) -> updater k v acc) input kvs
  ;;

  let update_input kvs =
    kvs |> CCList.map (fun (k, v) -> k, Some [ v ]) |> update_input_helper
  ;;

  let delete_from_input ks = ks |> CCList.map (fun k -> k, None) |> update_input_helper
end

let create_empty_data () =
  let open CCResult.Infix in
  let open Error in
  let input = [] in
  let experiment = Model.create_experiment () in
  let location = Location_test.create_location () in
  let res = SessionC.Create.(input |> decode >>= handle experiment location) in
  check_result
    (Error
       (Conformist
          Field.
            [ Start, NoValue
            ; Duration, NoValue
            ; TimeUnitOf Duration, NoValue
            ; MaxParticipants, NoValue
            ; MinParticipants, NoValue
            ; Overbook, NoValue
            ]))
    res
;;

let create_invalid_data () =
  let open CCResult.Infix in
  let open Field in
  let open Error in
  let open Data.Invalid in
  let experiment = Model.create_experiment () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(Data.invalid_input |> decode >>= handle experiment location)
  in
  check_result
    (Error
       (Conformist
          [ Start, NotADatetime (start, "1: unexpected end of input")
          ; Duration, NotANumber duration
          ; TimeUnitOf Duration, Invalid TimeUnit
          ; InternalDescription, NoValue
          ; PublicDescription, NoValue
          ; MaxParticipants, NotANumber max
          ; MinParticipants, NotANumber min
          ; Overbook, NotANumber overbook
          ; EmailLeadTime, NotANumber lead_time
          ]))
    res
;;

let create_min_gt_max () =
  let open CCResult.Infix in
  let open Field in
  let input = Data.update_input [ MaxParticipants, "5"; MinParticipants, "6" ] in
  let experiment = Model.create_experiment () in
  let location = Location_test.create_location () in
  let res = SessionC.Create.(input |> decode >>= handle experiment location) in
  check_result (Error (Error.Smaller (MaxParticipants, MinParticipants))) res
;;

let create_no_optional () =
  let open CCResult.Infix in
  let open Field in
  let session_id = Session.Id.create () in
  let input =
    let open Data in
    delete_from_input
      [ InternalDescription
      ; PublicDescription
      ; EmailLeadTime
      ; TextMessageLeadTime
      ; SentAt
      ; AssignmentCount
      ]
  in
  let experiment = Model.create_experiment () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle ~session_id experiment location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      start1
      duration
      location
      max_participants
      min_participants
      overbook
      experiment
  in
  check_result (Ok [ Pool_event.Session (Session.Created session) ]) res
;;

let create_full () =
  let open CCResult.Infix in
  let experiment = Model.create_experiment () in
  let session_id = Session.Id.create () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(Data.input |> decode >>= handle ~session_id experiment location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      ~email_reminder_lead_time
      ~internal_description
      ~public_description
      ~text_message_reminder_lead_time
      start1
      duration
      location
      max_participants
      min_participants
      overbook
      experiment
  in
  check_result (Ok [ Pool_event.Session (Session.Created session) ]) res
;;

let create_min_eq_max () =
  let open CCResult.Infix in
  let session_id = Session.Id.create () in
  let input = Data.update_input Field.[ MaxParticipants, "5"; MinParticipants, "5" ] in
  let experiment = Model.create_experiment () in
  let location = Location_test.create_location () in
  let res =
    SessionC.Create.(input |> decode >>= handle ~session_id experiment location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      ~email_reminder_lead_time
      ~internal_description
      ~public_description
      ~text_message_reminder_lead_time
      start1
      duration
      location
      max_participants2
      min_participants
      overbook
      experiment
  in
  check_result (Ok [ Pool_event.Session (Session.Created session) ]) res
;;

let update_empty_data () =
  let open CCResult.Infix in
  let open Error in
  let location = Location_test.create_location () in
  let session = Model.create_session () in
  let input = [] in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result
    (Error
       (let open Field in
        Conformist
          [ Start, NoValue
          ; Duration, NoValue
          ; TimeUnitOf Duration, NoValue
          ; MaxParticipants, NoValue
          ; MinParticipants, NoValue
          ; Overbook, NoValue
          ]))
    res
;;

(* TODO [aerben] test updating empty start & desc with has_assignments *)

let update_invalid_data () =
  let open CCResult.Infix in
  let open Field in
  let open Error in
  let open Data.Invalid in
  let location = Location_test.create_location () in
  let session = Model.create_session () in
  let res =
    SessionC.Update.(Data.invalid_input |> decode >>= handle [] session location)
  in
  check_result
    (Error
       (Conformist
          [ Start, NotADatetime (start, "1: unexpected end of input")
          ; Duration, NotANumber duration
          ; TimeUnitOf Duration, Invalid TimeUnit
          ; InternalDescription, NoValue
          ; PublicDescription, NoValue
          ; MaxParticipants, NotANumber max
          ; MinParticipants, NotANumber min
          ; Overbook, NotANumber overbook
          ; EmailLeadTime, NotANumber lead_time
          ]))
    res
;;

let update_min_gt_max () =
  let open CCResult.Infix in
  let open Field in
  let input = Data.update_input Field.[ MaxParticipants, "5"; MinParticipants, "6" ] in
  let session = Model.create_session () in
  let location = Location_test.create_location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  check_result (Error (Error.Smaller (MaxParticipants, MinParticipants))) res
;;

let update_no_optional () =
  let open CCResult.Infix in
  let input =
    Data.delete_from_input
      Field.
        [ InternalDescription
        ; PublicDescription
        ; EmailLeadTime
        ; TextMessageLeadTime
        ; SentAt
        ; AssignmentCount
        ]
  in
  let location = Location_test.create_location () in
  let session = Model.create_session ~location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = start1
      ; duration
      ; internal_description = None
      ; public_description = None
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = None
      ; text_message_reminder_lead_time = None
      }
  in
  check_result (Ok [ Pool_event.Session (Session.Updated (session, updated)) ]) res
;;

let update_full () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Model.create_session ~location () in
  let input =
    let open Data in
    update_input [ Field.Start, String.start2 ]
  in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = start2
      ; duration
      ; internal_description = Some internal_description
      ; public_description = Some public_description
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = Some email_reminder_lead_time
      ; text_message_reminder_lead_time = Some text_message_reminder_lead_time
      }
  in
  check_result (Ok [ Pool_event.Session (Session.Updated (session, updated)) ]) res
;;

let update_min_eq_max () =
  let open CCResult.Infix in
  let input = Data.update_input Field.[ MaxParticipants, "5"; MinParticipants, "5" ] in
  let location = Location_test.create_location () in
  let session = Model.create_session ~location () in
  let res = SessionC.Update.(input |> decode >>= handle [] session location) in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = start1
      ; duration
      ; internal_description = Some internal_description
      ; public_description = Some public_description
      ; max_participants = max_participants2
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = Some email_reminder_lead_time
      ; text_message_reminder_lead_time = Some text_message_reminder_lead_time
      }
  in
  check_result (Ok [ Pool_event.Session (Session.Updated (session, updated)) ]) res
;;

let delete () =
  let session = Model.create_session () in
  let res = SessionC.Delete.(handle { session; follow_ups = []; templates = [] }) in
  check_result (Ok [ Pool_event.Session (Session.Deleted session) ]) res
;;

let delete_closed_session () =
  let closed_at = Ptime_clock.now () in
  let session = Model.create_session () in
  let session = Session.{ session with closed_at = Some closed_at } in
  let res = SessionC.Delete.(handle { session; follow_ups = []; templates = [] }) in
  check_result
    (closed_at
     |> Pool_model.Time.formatted_date_time
     |> Error.sessionalreadyclosed
     |> CCResult.fail)
    res
;;

let delete_session_with_assignments () =
  let session = Model.create_session () in
  let session =
    Session.
      { session with assignment_count = AssignmentCount.create 1 |> CCResult.get_exn }
  in
  let res = SessionC.Delete.(handle { session; follow_ups = []; templates = [] }) in
  check_result (Error Error.SessionHasAssignments) res
;;

let delete_session_with_follow_ups () =
  let id = Session.Id.create () in
  let session = Model.create_session ~id () in
  let follow_up_session = Model.create_session ~follow_up_to:id () in
  let res =
    SessionC.Delete.(
      handle { session; follow_ups = [ follow_up_session ]; templates = [] })
  in
  check_result (Error Error.SessionHasFollowUps) res
;;

let create_email_job (experiment : Experiment.t) email =
  Email.Service.Job.create ?smtp_auth_id:experiment.Experiment.smtp_auth_id email
;;

let create_cancellation_message experiment reason contact =
  let recipient = contact |> Contact.email_address |> Pool_user.EmailAddress.value in
  let email = Model.create_email ~recipient () in
  let email =
    reason |> Session.CancellationReason.value |> flip Sihl_email.set_text email
  in
  create_email_job experiment email
  |> Email.create_dispatch
       ~job_ctx:
         (Pool_queue.job_ctx_create
            [ Contact.(contact |> id |> Id.to_common)
            ; Experiment.(experiment |> id |> Id.to_common)
            ])
  |> CCResult.return
;;

let create_cancellation_text_message
      (_ : Session.CancellationReason.t)
      (_ : Contact.t)
      cell_phone
  =
  Model.create_text_message_job cell_phone |> CCResult.return
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result (Error Error.(Conformist [ Field.Reason, NoValue ])) res
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [])
  in
  check_result (Error (Error.NoOptionSelected Field.NotifyVia)) res
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result (Error Error.SessionInPast) res
;;

let cancel_already_canceled () =
  let open CCResult.Infix in
  let now = Ptime_clock.now () in
  let session = Session.{ (Model.create_session ()) with canceled_at = Some now } in
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  check_result
    (now
     |> Pool_model.Time.formatted_date_time
     |> Error.sessionalreadycanceled
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [ Pool_common.NotifyVia.Email ])
  in
  let messages =
    assignments
    |> CCList.map (fun (contact, _) ->
      create_cancellation_message
        experiment
        (reason |> Session.CancellationReason.of_string)
        contact
      |> Test_utils.get_or_failwith
      |> Email.sent
      |> Pool_event.email)
  in
  check_result
    (Ok (messages @ [ Pool_event.Session (Session.Canceled session1) ] @ contact_events))
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
            assignments
            (create_cancellation_message experiment)
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
      |> get_or_failwith
      |> Text_message.sent
      |> Pool_event.text_message)
  in
  check_result
    (Ok (text_messags @ (Pool_event.Session (Session.Canceled session2) :: contact_events)))
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
      |> CCOption.to_result (Error.Invalid Field.CellPhone)
      |> Test_utils.get_or_failwith
      |> create_cancellation_text_message
           (Session.CancellationReason.of_string reason)
           contact1
      |> Test_utils.get_or_failwith
      |> Text_message.sent
      |> Pool_event.text_message
    in
    let email =
      create_cancellation_message
        experiment
        (reason |> Session.CancellationReason.of_string)
        contact2
      |> Test_utils.get_or_failwith
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            [ Pool_common.NotifyVia.TextMessage ])
  in
  check_result
    (Ok
       (messages @ [ Session.Canceled session |> Pool_event.session ] @ contact_events
        |> Test_utils.sort_events))
    (res |> CCResult.map Test_utils.sort_events)
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
        experiment
        (reason |> Session.CancellationReason.of_string)
        contact
      |> Test_utils.get_or_failwith
      |> Email.sent
      |> Pool_event.email
    in
    let text_msg contact =
      contact.Contact.cell_phone
      |> CCOption.to_result (Error.Invalid Field.CellPhone)
      |> Test_utils.get_or_failwith
      |> create_cancellation_text_message
           (Session.CancellationReason.of_string reason)
           contact
      |> Test_utils.get_or_failwith
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
            assignments
            (create_cancellation_message experiment)
            create_cancellation_text_message
            Pool_common.NotifyVia.[ TextMessage; Email ])
  in
  check_result
    (Ok
       (messages @ [ Session.Canceled session |> Pool_event.session ] @ contact_events
        |> Test_utils.sort_events))
    (res |> CCResult.map Test_utils.sort_events)
;;

let close_valid () =
  let experiment = Test_utils.Model.create_experiment () in
  let open Cqrs_command.Session_command.Close in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let res = handle experiment session [] [] in
  check_result (Ok [ Session.Closed session |> Pool_event.session ]) res
;;

let close_valid_with_assignments () =
  let experiment = Test_utils.Model.create_experiment () in
  let open Cqrs_command.Session_command in
  let open Assignment in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let assignments =
    [ true ]
    |> CCList.map (fun participated ->
      ()
      |> Test_utils.Model.create_contact
      |> create
           ~no_show:(NoShow.create false)
           ~participated:(Participated.create participated)
      |> fun assignment ->
      assignment, Assignment.IncrementParticipationCount.create true, None)
  in
  let tags = Tag_test.Data.Tag.create_with_description () |> CCList.return in
  let res = Close.handle experiment session tags assignments in
  let expected =
    CCList.fold_left
      (fun events ((assignment : Assignment.t), _, (_ : t list option)) ->
         let contact_event =
           let open Contact in
           let contact =
             assignment.contact
             |> update_num_show_ups ~step:1
             |> update_num_participations ~step:1
           in
           Contact.Updated contact |> Pool_event.contact
         in
         let tag_events =
           let open Tags in
           tags
           |> CCList.map (fun (tag : t) ->
             Tagged
               { Tagged.model_uuid = Contact.id assignment.contact |> Contact.Id.to_common
               ; tag_uuid = tag.id
               }
             |> Pool_event.tags)
         in
         events
         @ [ Updated (assignment, assignment) |> Pool_event.assignment; contact_event ]
         @ tag_events)
      [ Session.Closed session |> Pool_event.session ]
      assignments
    |> CCResult.return
  in
  check_result expected res
;;

let close_with_deleted_assignment () =
  let experiment = Test_utils.Model.create_experiment () in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let command =
    let open Assignment in
    let base = Test_utils.Model.create_assignment () in
    let assignment =
      { base with
        marked_as_deleted = MarkedAsDeleted.create true
      ; no_show = Some (NoShow.create false)
      ; participated = Some (Participated.create true)
      }
    in
    assignment, Assignment.IncrementParticipationCount.create false, None
  in
  let res = Cqrs_command.Session_command.Close.handle experiment session [] [ command ] in
  check_result (Error (Error.IsMarkedAsDeleted Field.Assignment)) res
;;

let validate_invalid_participation () =
  let open Cqrs_command.Session_command.Close in
  let open Assignment in
  let experiment = Test_utils.Model.create_experiment () in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let assignment =
    Test_utils.Model.create_contact ()
    |> create ~no_show:(NoShow.create true) ~participated:(Participated.create true)
  in
  let participation =
    assignment, Assignment.IncrementParticipationCount.create false, None
  in
  let res = handle experiment session [] [ participation ] in
  let expected = Error Error.AssignmentsHaveErrors in
  check_result expected res
;;

let close_unparticipated_with_followup () =
  let open Cqrs_command.Session_command.Close in
  let open Test_utils in
  let open Assignment in
  let experiment = Test_utils.Model.create_experiment () in
  let session = Test_utils.Model.(create_session ~start:(an_hour_ago ())) () in
  let contact = Model.create_contact () in
  let assignment = Model.create_assignment ~contact () in
  let assignment =
    { assignment with
      no_show = Some (NoShow.create false)
    ; participated = Some (Participated.create false)
    }
  in
  let follow_up = Model.create_assignment ~contact () in
  let participation =
    assignment, Assignment.IncrementParticipationCount.create true, Some [ follow_up ]
  in
  let res = handle experiment session [] [ participation ] in
  let expected =
    let contact =
      let open Contact in
      contact |> update_num_show_ups ~step:1 |> update_num_participations ~step:1
    in
    Ok
      [ Session.Closed session |> Pool_event.session
      ; Assignment.Updated (assignment, assignment) |> Pool_event.assignment
      ; Contact.Updated contact |> Pool_event.contact
      ; Assignment.MarkedAsDeleted follow_up |> Pool_event.assignment
      ]
  in
  check_result expected res
;;

let create_follow_up_earlier () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Test_utils.Model.create_session ~location () in
  let experiment = Model.create_experiment () in
  let res =
    SessionC.Create.(
      Data.input |> decode >>= handle ~parent_session:session experiment location)
  in
  check_result (Error Error.FollowUpIsEarlierThanMain) res
;;

let create_follow_up_later () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let parent_session = Test_utils.Model.create_session ~location () in
  let session_id = Session.Id.create () in
  let experiment = Model.create_experiment () in
  let later_start =
    parent_session.Session.start
    |> Session.Start.value
    |> flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    Data.update_input [ Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start ]
  in
  let res =
    SessionC.Create.(
      input |> decode >>= handle ~session_id ~parent_session experiment location)
  in
  let session =
    let open Data.Validated in
    Session.create
      ~id:session_id
      ~email_reminder_lead_time
      ~internal_description
      ~follow_up_to:parent_session.Session.id
      (Session.Start.create later_start)
      ~public_description
      ~text_message_reminder_lead_time
      duration
      location
      max_participants
      min_participants
      overbook
      experiment
  in
  check_result (Ok [ Pool_event.Session (Session.Created session) ]) res
;;

let update_follow_up_earlier () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Test_utils.Model.create_session ~location () in
  let res =
    SessionC.Update.(
      Data.input |> decode >>= handle ~parent_session:session [] session location)
  in
  check_result (Error Error.FollowUpIsEarlierThanMain) res
;;

let update_follow_up_later () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  let session = Test_utils.Model.create_session ~location () in
  let later_start =
    session.Session.start
    |> Session.Start.value
    |> flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    Data.update_input [ Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start ]
  in
  let res =
    SessionC.Update.(
      input |> decode >>= handle ~parent_session:session [] session location)
  in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = Session.Start.create later_start
      ; duration
      ; internal_description = Some internal_description
      ; public_description = Some public_description
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = Some email_reminder_lead_time
      ; text_message_reminder_lead_time = Some text_message_reminder_lead_time
      }
  in
  check_result (Ok [ Pool_event.Session (Session.Updated (session, updated)) ]) res
;;

let update_follow_ups_earlier () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  (* Valid starting setup, main session happens before two follow-ups *)
  let session =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start1
    }
  in
  let follow_up1 =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start2
    }
  in
  let follow_up2 =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start3
    }
  in
  (* Make input start after first follow-up *)
  let later_start1 =
    Data.Validated.start2
    |> Session.Start.value
    |> flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
    |> Ptime.to_rfc3339 ~frac_s:12
  in
  let input1 = Data.update_input [ Field.Start, later_start1 ] in
  let res_earlier_one =
    SessionC.Update.(
      input1 |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result (Error Error.FollowUpIsEarlierThanMain) res_earlier_one;
  (* Make input start after both follow-ups *)
  let later_start2 =
    Data.Validated.start3
    |> Session.Start.value
    |> flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
    |> Ptime.to_rfc3339 ~frac_s:12
  in
  let input2 =
    let open Data in
    update_input [ Field.Start, later_start2 ]
  in
  let res_earlier_all =
    SessionC.Update.(
      input2 |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  check_result (Error Error.FollowUpIsEarlierThanMain) res_earlier_all
;;

let update_follow_ups_later () =
  let open CCResult.Infix in
  let location = Location_test.create_location () in
  (* Valid starting setup, main session happens before two follow-ups *)
  let session =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start1
    }
  in
  let follow_up1 =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start2
    }
  in
  let follow_up2 =
    { (Test_utils.Model.create_session ~location ()) with
      Session.start = Data.Validated.start3
    }
  in
  let res_normal =
    SessionC.Update.(
      Data.input |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = start1
      ; duration
      ; internal_description = Some internal_description
      ; public_description = Some public_description
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = Some email_reminder_lead_time
      ; text_message_reminder_lead_time = Some text_message_reminder_lead_time
      }
  in
  check_result (Ok [ Pool_event.Session (Session.Updated (session, updated)) ]) res_normal;
  (* Make input start later, but before both follow-ups *)
  let later_start =
    Data.Validated.start1
    |> Session.Start.value
    |> flip Ptime.add_span @@ Ptime.Span.of_int_s (60 * 60)
    |> CCOption.get_exn_or "Invalid new start"
  in
  let input =
    Data.update_input [ Field.Start, Ptime.to_rfc3339 ~frac_s:12 later_start ]
  in
  let res_later_but_earlier =
    SessionC.Update.(
      input |> decode >>= handle [ follow_up1; follow_up2 ] session location)
  in
  let updated =
    let open Data.Validated in
    Session.
      { session with
        start = Start.create later_start
      ; duration
      ; internal_description = Some internal_description
      ; public_description = Some public_description
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time = Some email_reminder_lead_time
      ; text_message_reminder_lead_time = Some text_message_reminder_lead_time
      }
  in
  check_result
    (Ok [ Pool_event.Session (Session.Updated (session, updated)) ])
    res_later_but_earlier
;;

let reschedule_to_past () =
  let session = Test_utils.Model.create_session () in
  let experiment = Model.create_experiment () in
  let create_message _ _ _ =
    Test_utils.Model.create_email ()
    |> create_email_job experiment
    |> Email.create_dispatch
         ~job_ctx:
           (Pool_queue.job_ctx_create
              [ Session.(session.id |> Id.to_common)
              ; Experiment.(experiment |> id |> Id.to_common)
              ])
    |> CCResult.return
  in
  let command =
    let open Session in
    SessionC.
      { start =
          Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s @@ (60 * 60))
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; duration =
          Duration.to_int_s session.duration |> CCOption.get_exn_or "Invalid duration"
      ; duration_unit = Duration.with_largest_unit session.duration |> fst
      }
  in
  let events = SessionC.Reschedule.handle [] session [] create_message command in
  let expected = Error Error.TimeInPast in
  Test_utils.check_result expected events
;;

let reschedule_with_experiment_smtp () =
  let session = Test_utils.Model.create_session () in
  let experiment = Model.create_experiment () in
  let smtp_auth_id = Email.SmtpAuth.Id.create () in
  let experiment = { experiment with Experiment.smtp_auth_id = Some smtp_auth_id } in
  let assignment = Test_utils.Model.create_assignment () in
  let email_to_job =
    Email.create_dispatch
      ~job_ctx:
        (Pool_queue.job_ctx_create
           [ Session.(session.id |> Id.to_common)
           ; Experiment.(experiment |> id |> Id.to_common)
           ])
  in
  let create_message _ _ _ =
    Test_utils.Model.create_email ()
    |> create_email_job experiment
    |> email_to_job
    |> CCResult.return
  in
  let command =
    let open Session in
    SessionC.
      { start = Test_utils.Model.in_two_hours ()
      ; duration =
          Duration.to_int_s session.duration |> CCOption.get_exn_or "Invalid duration"
      ; duration_unit = Duration.with_largest_unit session.duration |> fst
      }
  in
  let rescheduled { SessionC.start; duration; duration_unit } =
    Session.
      { start
      ; duration = Session.Duration.of_int duration duration_unit |> get_or_failwith
      }
  in
  let events =
    SessionC.Reschedule.handle [] session [ assignment ] create_message command
  in
  let expected =
    let email = Test_utils.Model.create_email () in
    let job = create_email_job experiment email |> email_to_job in
    Ok
      [ Session.Rescheduled (session, rescheduled command) |> Pool_event.session
      ; Email.BulkSent [ job ] |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;

let resend_reminders_invalid () =
  let open Cqrs_command.Session_command.ResendReminders in
  let open Test_utils in
  let experiment = Model.create_experiment () in
  let assignments = [] in
  let create_email assignment =
    Model.create_email ()
    |> create_email_job experiment
    |> Email.create_dispatch
         ~job_ctx:
           (Pool_queue.job_ctx_create
              [ Assignment.(assignment.id |> Id.to_common)
              ; Experiment.(experiment |> id |> Id.to_common)
              ])
    |> CCResult.return
  in
  let create_text_message _ cell_phone = Ok (Model.create_text_message_job cell_phone) in
  let channel = Pool_common.MessageChannel.Email in
  let session = Model.create_session () in
  let canceled_at = Ptime_clock.now () in
  let session1 = Session.{ session with canceled_at = Some canceled_at } in
  let closed_at = Ptime_clock.now () in
  let session2 = Session.{ session with closed_at = Some closed_at } in
  let handle session =
    handle (create_email, create_text_message) session assignments channel
  in
  let res1 = handle session1 in
  let () =
    check_result
      (Error
         (Error.SessionAlreadyCanceled (Pool_model.Time.formatted_date_time canceled_at)))
      res1
  in
  let res2 = handle session2 in
  let () =
    check_result
      (Error (Error.SessionAlreadyClosed (Pool_model.Time.formatted_date_time closed_at)))
      res2
  in
  ()
;;

let resend_reminders_valid () =
  let open Cqrs_command.Session_command.ResendReminders in
  let open Pool_common in
  let open Test_utils in
  let experiment = Model.create_experiment () in
  let session = Model.create_session () in
  let cell_phone = Pool_user.CellPhone.of_string "+41791234567" in
  let contact1 = Model.create_contact () in
  let contact2 = Contact.{ (Model.create_contact ()) with cell_phone = None } in
  let assignments =
    [ contact1; contact2 ]
    |> CCList.map (fun contact -> Model.create_assignment ~contact ())
  in
  let create_email _ =
    Model.create_email ()
    |> create_email_job experiment
    |> Email.create_dispatch
         ~job_ctx:
           (Pool_queue.job_ctx_create
              [ Session.(session.id |> Id.to_common)
              ; Experiment.(experiment |> id |> Id.to_common)
              ])
    |> CCResult.return
  in
  let create_text_message _ cell_phone = Ok (Model.create_text_message_job cell_phone) in
  let handle channel =
    handle (create_email, create_text_message) session assignments channel
  in
  let res1 = handle MessageChannel.Email in
  let expected1 =
    let emails =
      assignments |> CCList.map create_email |> CCList.all_ok |> get_or_failwith
    in
    Ok
      [ Session.EmailReminderSent session |> Pool_event.session
      ; Email.BulkSent emails |> Pool_event.email
      ]
  in
  let () = check_result expected1 res1 in
  let res2 = handle MessageChannel.TextMessage in
  let expected2 =
    Ok
      [ Email.BulkSent [ create_email (assignments |> CCList.hd) |> CCResult.get_exn ]
        |> Pool_event.email
      ; Text_message.BulkSent [ Model.create_text_message_job cell_phone ]
        |> Pool_event.text_message
      ; Session.TextMsgReminderSent session |> Pool_event.session
      ]
  in
  let () = check_result expected2 res2 in
  ()
;;

let close_session_check_contact_figures _ () =
  let open Utils.Lwt_result.Infix in
  let open Integration_utils in
  let open Test_utils in
  let%lwt current_user = current_user () in
  let%lwt experiment = Repo.first_experiment () in
  let%lwt session = SessionRepo.create ~start:(Model.an_hour_ago ()) experiment () in
  let%lwt participated_c = ContactRepo.create ~with_terms_accepted:true () in
  let%lwt show_up_c = ContactRepo.create ~with_terms_accepted:true () in
  let%lwt no_show_c = ContactRepo.create ~with_terms_accepted:true () in
  let contacts =
    [ participated_c, `Participated; show_up_c, `ShowUp; no_show_c, `NoShow ]
  in
  let%lwt (_ : Assignment.t list) =
    contacts |> Lwt_list.map_s (fst %> AssignmentRepo.create session)
  in
  let%lwt assignments =
    Assignment.find_not_deleted_by_session Data.database_label session.Session.id
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
    let open Assignment in
    contacts
    |> map (fun (contact, status) ->
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
        |> Test_utils.get_or_failwith
      in
      let assignment = find_assignment contact in
      let updated =
        { assignment with no_show = Some no_show; participated = Some participated }
      in
      [ Updated (assignment, updated) |> Pool_event.assignment
      ; Contact.Updated contact |> Pool_event.contact
      ])
    |> flatten
    |> cons (Session.Closed session |> Pool_event.session)
    |> Pool_event.handle_events Data.database_label current_user
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
        ||> get_or_failwith
      in
      (NumberOfShowUps.equal contact.num_show_ups num_show_ups
       && NumberOfNoShows.equal contact.num_no_shows num_no_shows
       && NumberOfParticipations.equal contact.num_participations num_participations)
      |> Lwt.return)
    ||> CCList.filter not
    ||> CCList.is_empty
  in
  let () = Alcotest.(check bool "succeeds" true res) in
  Lwt.return_unit
;;

let send_session_reminders_with_default_leat_time _ () =
  let open Utils.Lwt_result.Infix in
  let open Integration_utils in
  let get_exn = get_or_failwith in
  let database_label = Test_utils.Data.database_label in
  let%lwt tenant = Pool_tenant.find_by_label database_label ||> get_exn in
  (* NOTE: disable text messages for test *)
  let tenant = { tenant with Pool_tenant.text_messages_enabled = false } in
  let s_to_lead encode s = s |> Ptime.Span.of_int_s |> encode |> get_exn in
  let%lwt () =
    Pool_common.Reminder.
      [ Settings.DefaultReminderLeadTimeUpdated
          (24 * 60 * 60 |> s_to_lead EmailLeadTime.create)
      ; Settings.DefaultTextMsgReminderLeadTimeUpdated
          (12 * 60 * 60 |> s_to_lead TextMessageLeadTime.create)
      ]
    |> Lwt_list.iter_s (Settings.handle_event database_label)
  in
  let%lwt experiment = ExperimentRepo.create () in
  let create_session ?email_reminder_sent_at hours =
    let start =
      hours * 60 * 60 |> Ptime.Span.of_int_s |> Test_utils.Model.session_start_in
    in
    SessionRepo.create ~start ?email_reminder_sent_at experiment ()
  in
  let%lwt session1 = create_session 16 in
  let%lwt session2 =
    let email_reminder_sent_at =
      let hour = Ptime.Span.of_int_s @@ (60 * 60) in
      Ptime.sub_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid timespan"
      |> Pool_common.Reminder.SentAt.create
    in
    create_session ~email_reminder_sent_at 8
  in
  let%lwt contact = ContactRepo.create () in
  let%lwt assignment1 = AssignmentRepo.create session1 contact in
  let%lwt assignment2 = AssignmentRepo.create session2 contact in
  let update_session { Session.id; _ } = Session.find database_label id ||> get_exn in
  let%lwt session1 = update_session session1 in
  let%lwt session2 = update_session session2 in
  let%lwt _, text_message_reminders =
    Session.find_sessions_to_remind tenant ||> get_exn
  in
  (* Expect list to be empty as no GTX API Key is set *)
  let () =
    CCList.is_empty text_message_reminders |> Alcotest.(check bool) "succeeds" true
  in
  let%lwt () =
    let open Pool_tenant in
    let%lwt write = Pool_tenant.find_full tenant.id ||> get_exn in
    GtxApiKeyUpdated (write, (GtxApiKey.of_string "api-key", GtxSender.of_string "sender"))
    |> handle_event tenant.database_label
  in
  let%lwt tenant = Pool_tenant.find_by_label database_label ||> get_exn in
  let%lwt email_reminders, text_message_reminders =
    Session.find_sessions_to_remind tenant
    ||> get_exn
    ||> fun (email_reminders, text_message_reminders) ->
    let open Session in
    (* Make sure only relevant sessions are returned *)
    let filter =
      CCList.filter (fun { id; _ } -> CCList.mem id [ session1.id; session2.id ])
    in
    filter email_reminders, filter text_message_reminders
  in
  let%lwt res =
    Reminder.Service.create_reminder_events tenant email_reminders text_message_reminders
  in
  let%lwt expected =
    let open Message_template in
    let%lwt sys_languages = Settings.find_languages database_label in
    let%lwt emails =
      let%lwt create =
        SessionReminder.prepare_emails
          database_label
          tenant
          sys_languages
          experiment
          session1
      in
      [ Email.BulkSent [ create assignment1 |> get_exn ] |> Pool_event.email
      ; Session.EmailReminderSent session1 |> Pool_event.session
      ]
      |> Lwt.return
    in
    let%lwt text_messages =
      let cell_phone =
        contact.Contact.cell_phone |> CCOption.get_exn_or "Missing cell phone"
      in
      let%lwt create =
        Message_template.SessionReminder.prepare_text_messages
          database_label
          tenant
          sys_languages
          experiment
          session2
      in
      [ Text_message.BulkSent [ create assignment2 cell_phone |> get_exn ]
        |> Pool_event.text_message
      ; Session.TextMsgReminderSent session2 |> Pool_event.session
      ]
      |> Lwt.return
    in
    Lwt_result.return (emails @ text_messages)
  in
  let () = check_result expected res in
  Lwt.return_unit
;;

module Duplication = struct
  open Test_utils
  open Session

  let equal_start a b =
    let open Ptime in
    let open Session.Start in
    Ptime.diff (value a) (value b) |> Span.to_float_s |> CCFloat.abs < 1.
  ;;

  let equal_session (a : t) (b : t) =
    let open Session in
    let open Pool_common.Reminder in
    (* The ID's are generated in the command handle function, therefore we do not compare
       the ids but check wheater 'follow_up_to is set or not' *)
    let equal_id _ _ = true in
    let amount_equal = ParticipantAmount.equal in
    CCOption.equal equal_id a.follow_up_to b.follow_up_to
    && CCOption.equal
         InternalDescription.equal
         a.internal_description
         b.internal_description
    && CCOption.equal PublicDescription.equal a.public_description b.public_description
    && CCOption.equal
         EmailLeadTime.equal
         a.email_reminder_lead_time
         b.email_reminder_lead_time
    && CCOption.equal
         TextMessageLeadTime.equal
         a.text_message_reminder_lead_time
         b.text_message_reminder_lead_time
    && equal_start a.start b.start
    && Duration.equal a.duration b.duration
    && Pool_location.equal a.location b.location
    && amount_equal a.max_participants b.max_participants
    && amount_equal a.min_participants b.min_participants
    && amount_equal a.overbook b.overbook
  ;;

  let testable_session = Alcotest.testable Session.pp equal_session
  let expect_error = check_result

  let check_sessions =
    let open Alcotest in
    check (result (list testable_session) error) "succeeds"
  ;;

  let[@warning "-4"] get_event_sessions =
    let open Session in
    CCList.filter_map (function
      | Pool_event.Session (Created session) -> Some session
      | _ -> None)
  ;;

  let input_name group session =
    Format.asprintf "%s[%i]" Session.(Id.value session.id) group
  ;;

  let handle_timespan_update fnc ({ Session.start; _ } : Session.t) timespan =
    fnc (Session.Start.value start) timespan
    |> CCOption.get_exn_or "Invalid timespan provided"
    |> Session.Start.create
  ;;

  let add_timespan = handle_timespan_update Ptime.add_span
  let sub_timespan = handle_timespan_update Ptime.sub_span

  let start_to_string start =
    start |> Session.Start.value |> Pool_model.Base.Ptime.date_time_to_flatpickr
  ;;

  let data_to_urlencded =
    CCList.map (fun (session, group, start) ->
      input_name group session, [ start_to_string start ])
  ;;

  let to_event session = Session.Created session |> Pool_event.session

  let single_session () =
    let session = Model.create_session () in
    let data =
      [ session, 0, add_timespan session Model.hour
      ; session, 1, add_timespan session Model.hour
      ]
    in
    let events =
      data
      |> data_to_urlencded
      |> SessionC.Duplicate.handle session []
      |> CCResult.map get_event_sessions
    in
    let expected =
      data
      |> CCList.map (fun (session, _, start) ->
        Session.{ session with id = Session.Id.create (); start })
      |> CCResult.return
    in
    check_sessions expected events
  ;;

  let with_followup () =
    let session = Model.create_session () in
    let followup =
      let session =
        Model.create_session
          ~follow_up_to:session.id
          ~start:(add_timespan session Model.hour)
          ()
      in
      let create_amount i = i |> ParticipantAmount.create |> get_or_failwith in
      { session with
        min_participants = create_amount 1
      ; max_participants = create_amount 1
      ; overbook = create_amount 0
      }
    in
    let data =
      [ session, 0, add_timespan session Model.hour
      ; followup, 0, add_timespan followup Model.hour
      ]
    in
    let events =
      data
      |> data_to_urlencded
      |> SessionC.Duplicate.handle session [ followup ]
      |> CCResult.map get_event_sessions
    in
    let expected =
      data
      |> CCList.map (fun (session, _, start) ->
        Session.{ session with id = Session.Id.create (); start })
      |> CCResult.return
    in
    check_sessions expected events
  ;;

  let missing_value () =
    let session = Model.create_session () in
    let followup =
      Model.create_session
        ~follow_up_to:session.id
        ~start:(add_timespan session Model.hour)
        ()
    in
    let data = [ session, 0, add_timespan session Model.hour ] in
    let events =
      data |> data_to_urlencded |> SessionC.Duplicate.handle session [ followup ]
    in
    expect_error (Error (Error.Missing Field.Start)) events
  ;;

  let followup_before_main () =
    let session = Model.create_session () in
    let followup =
      Model.create_session
        ~follow_up_to:session.id
        ~start:(add_timespan session Model.hour)
        ()
    in
    let data =
      [ session, 0, add_timespan session Model.hour
      ; followup, 0, sub_timespan session Model.hour
      ]
    in
    let events =
      data |> data_to_urlencded |> SessionC.Duplicate.handle session [ followup ]
    in
    expect_error (Error Error.FollowUpIsEarlierThanMain) events
  ;;
end

module DirectMessaging = struct
  open Cqrs_command.Session_command.SendDirectMessage
  open Message_template

  let language = Pool_common.Language.En
  let sms_text = SmsText.of_string "content"
  let email_text = EmailText.of_string "<p>content</p>"
  let plain_text = PlainText.of_string "content"
  let email_subject = EmailSubject.of_string "subject"
  let contact = Contact.{ (Model.create_contact ()) with cell_phone = None }
  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string

  let contact_with_cellphone =
    { (Model.create_contact ()) with Contact.cell_phone = Some cell_phone }
  ;;

  let make_email_job (_ : Assignment.t) (_ : Message_template.ManualMessage.t) =
    Model.create_email_job ()
  ;;

  let make_text_message_job
        (_ : Pool_common.Language.t)
        (_ : Assignment.t)
        (_ : Message_template.SmsText.t)
        cell_phone
    =
    Model.create_text_message_job cell_phone
  ;;

  let decode_and_handle assignments channel data =
    let open CCResult in
    data
    |> Http_utils.format_request_boolean_values [ Field.(show FallbackToEmail) ]
    |> decode channel
    >>= handle make_email_job make_text_message_job assignments
  ;;

  module UrlEncoded = struct
    let language = Field.(show Language), [ Pool_common.Language.show language ]
    let sms_text = Field.(show SmsText), [ SmsText.value sms_text ]
    let email_text = Field.(show EmailText), [ EmailText.value email_text ]
    let plain_text = Field.(show PlainText), [ PlainText.value plain_text ]
    let fallback_to_email = Field.(show FallbackToEmail), [ "true" ]
    let email_subject = Field.(show EmailSubject), [ EmailSubject.value email_subject ]
  end

  let send_emails () =
    let open CCResult in
    let assignments =
      [ Model.create_assignment ~contact ()
      ; Model.create_assignment ~contact:contact_with_cellphone ()
      ]
    in
    let channel = Pool_common.MessageChannel.Email in
    let res =
      UrlEncoded.[ language; email_subject; email_text; plain_text ]
      |> decode_and_handle assignments channel
    in
    let expected =
      Ok
        [ assignments
          |> CCList.map (CCFun.const (Model.create_email_job ()))
          |> Email.bulksent
          |> Pool_event.email
        ]
    in
    check_result expected res
  ;;

  let send_text_message_with_fallback () =
    let open CCResult in
    let assignments =
      [ Model.create_assignment ~contact ()
      ; Model.create_assignment ~contact:contact_with_cellphone ()
      ]
    in
    let channel = Pool_common.MessageChannel.TextMessage in
    let res =
      UrlEncoded.[ language; email_subject; sms_text; fallback_to_email ]
      |> decode_and_handle assignments channel
    in
    let expected =
      let text_msg =
        Text_message.BulkSent [ Model.create_text_message_job cell_phone ]
        |> Pool_event.text_message
      in
      let email_job =
        [ Model.create_email_job () ] |> Email.bulksent |> Pool_event.email
      in
      Ok [ text_msg; email_job ]
    in
    check_result expected res
  ;;

  let send_text_message_without_fallback () =
    let open CCResult in
    let assignments =
      [ Model.create_assignment ~contact ()
      ; Model.create_assignment ~contact:contact_with_cellphone ()
      ]
    in
    let channel = Pool_common.MessageChannel.TextMessage in
    let res =
      UrlEncoded.[ language; email_subject; sms_text ]
      |> decode_and_handle assignments channel
    in
    let expected =
      Ok
        [ Text_message.BulkSent [ Model.create_text_message_job cell_phone ]
          |> Pool_event.text_message
        ]
    in
    check_result expected res
  ;;
end
