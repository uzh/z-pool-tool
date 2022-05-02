module InvitationCommand = Cqrs_command.Invitation_command
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
          ; password = "somepassword"
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

let create_experiment () =
  let show_error err = Pool_common.(Utils.error_to_string Language.En err) in
  Experiment.
    { id = Pool_common.Id.create ()
    ; title =
        Title.create "An Experiment"
        |> CCResult.map_err show_error
        |> CCResult.get_or_failwith
    ; description =
        Description.create "A description for everyone"
        |> CCResult.map_err show_error
        |> CCResult.get_or_failwith
    ; filter = "*"
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

let create () =
  let experiment = create_experiment () in
  let subject = create_subject () in
  let events =
    let command = InvitationCommand.Create.{ experiment; subject } in
    InvitationCommand.Create.handle command
  in
  let expected =
    Ok
      [ Invitation.(
          Created { experiment_id = experiment.Experiment.id; subject })
        |> Pool_event.invitation
      ]
  in
  check_result expected events
;;
