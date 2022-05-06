module SubjectCommand = Cqrs_command.Subject_command
module ParticipationCommand = Cqrs_command.Participation_command
module Field = Pool_common.Message.Field

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let create_subject () =
  Subject.
    { user =
        Sihl_user.
          { id = Pool_common.Id.(create () |> value)
          ; email = "test@econ.uzh.ch"
          ; username = None
          ; name = None
          ; given_name = None
          ; password =
              "somepassword"
              |> Sihl_user.Hashing.hash
              |> CCResult.get_or_failwith
          ; status =
              Sihl_user.status_of_string "active" |> CCResult.get_or_failwith
          ; admin = false
          ; confirmed = true
          ; created_at = Pool_common.CreatedAt.create ()
          ; updated_at = Pool_common.UpdatedAt.create ()
          }
    ; recruitment_channel = RecruitmentChannel.Friend
    ; terms_accepted_at = Pool_user.TermsAccepted.create_now ()
    ; language = Some Pool_common.Language.En
    ; paused = Pool_user.Paused.create false
    ; disabled = Pool_user.Disabled.create false
    ; verified = Pool_user.Verified.create None
    ; email_verified =
        Pool_user.EmailVerified.create (Some (Ptime_clock.now ()))
    ; num_invitations = NumberOfInvitations.init
    ; num_assignments = NumberOfAssignments.init
    ; firstname_version = Pool_common.Version.create ()
    ; lastname_version = Pool_common.Version.create ()
    ; paused_version = Pool_common.Version.create ()
    ; language_version = Pool_common.Version.create ()
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create_session () =
  let hour = 60 * 60 in
  Session.
    { id = Pool_common.Id.create ()
    ; start =
        Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s hour)
        |> CCOption.get_exn_or "Invalid start"
    ; duration = Ptime.Span.of_int_s hour
    ; description = None
    ; max_participants =
        ParticipantAmount.create Pool_common.Message.Field.MaxParticipants "30"
        |> Pool_common.Utils.get_or_failwith
    ; min_participants =
        ParticipantAmount.create Pool_common.Message.Field.MinParticipants "1"
        |> Pool_common.Utils.get_or_failwith
    ; overbook =
        ParticipantAmount.create Pool_common.Message.Field.Overbook "4"
        |> Pool_common.Utils.get_or_failwith
    ; participant_count =
        ParticipantAmount.create Pool_common.Message.Field.ParticipantCount "0"
        |> Pool_common.Utils.get_or_failwith
    ; canceled_at = None
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create_participation () =
  Participation.
    { id = Pool_common.Id.create ()
    ; subject = create_subject ()
    ; show_up = ShowUp.init
    ; participated = Participated.init
    ; matches_filter = MatchesFilter.init
    ; canceled_at = CanceledAt.init
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create () =
  let session = create_session () in
  let subject = create_subject () in
  let events =
    let command = ParticipationCommand.Create.{ subject; session } in
    ParticipationCommand.Create.handle command
  in
  let expected =
    Ok
      [ Participation.(Created { subject; session_id = session.Session.id })
        |> Pool_event.participation
      ]
  in
  check_result expected events
;;

let canceled () =
  let participation = create_participation () in
  let events = ParticipationCommand.Cancel.handle participation in
  let expected =
    Ok [ Participation.Canceled participation |> Pool_event.participation ]
  in
  check_result expected events
;;

let set_attendance () =
  let participation = create_participation () in
  let show_up = "true" in
  let participated = "false" in
  let events =
    let attendance =
      Pool_common.Utils.get_or_failwith
      @@ ParticipationCommand.SetAttendance.decode
           [ Field.(ShowUp |> show), show_up |> CCList.pure
           ; Field.(Participated |> show), participated |> CCList.pure
           ]
    in
    ParticipationCommand.SetAttendance.handle participation attendance
  in
  let expected =
    let open Participation in
    Ok
      [ ShowedUp (participation, show_up |> bool_of_string |> ShowUp.create)
        |> Pool_event.participation
      ; Participated
          (participation, participated |> bool_of_string |> Participated.create)
        |> Pool_event.participation
      ]
  in
  check_result expected events
;;
