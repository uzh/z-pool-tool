open CCFun
module MatcherCommand = Cqrs_command.Matcher_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let get_or_failwith = Test_utils.get_or_failwith_pool_error

let sort_events =
  CCList.stable_sort Pool_event.(fun a b -> CCString.compare (show a) (show b))
;;

let expected_events
  ({ Experiment.smtp_auth_id; _ } as experiment)
  contacts
  create_message
  =
  let emails =
    CCList.map create_message contacts
    |> CCResult.(flatten_l %> get_exn)
    |> CCList.map (fun msg -> msg, smtp_auth_id)
  in
  let events =
    [ Invitation.Created (contacts, experiment) |> Pool_event.invitation
    ; Email.BulkSent emails |> Pool_event.email
    ]
    @ CCList.map
        Contact.(
          update_num_invitations ~step:1 %> updated %> Pool_event.contact)
        contacts
    |> sort_events
  in
  Ok events
;;

let create_message ?sender (_ : Contact.t) =
  let sender =
    sender
    |> CCOption.map_or ~default:"it@econ.uzh.ch" Pool_user.EmailAddress.value
  in
  Sihl_email.
    { sender
    ; recipient = "contact@econ.uzh.ch"
    ; subject = "Invitation"
    ; text = ""
    ; html = None
    ; cc = []
    ; bcc = []
    }
  |> CCResult.return
;;

let create_invitations_model () =
  let open Cqrs_command.Matcher_command.Run in
  let mailing = Model.create_mailing () in
  let experiment = Model.create_experiment () in
  let contacts = Model.[ create_contact (); create_contact () ] in
  let events =
    { mailing; experiment; contacts; create_message }
    |> CCList.pure
    |> handle
    |> CCResult.map sort_events
  in
  let expected = expected_events experiment contacts create_message in
  Test_utils.check_result expected events
;;

let create_invitations_repo _ () =
  let open Utils.Lwt_result.Infix in
  let pool = Test_utils.Data.database_label in
  let find_invitation_count { Experiment.id; _ } =
    Invitation.find_by_experiment pool id
    ||> get_or_failwith
    ||> fst
    ||> CCList.length
  in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt { Experiment.id; _ } = Test_utils.Repo.first_experiment () in
  let rate = Mailing.Rate.create 100 |> get_or_failwith in
  let mailing =
    let open Mailing in
    let distribution =
      Some
        Distribution.(Sorted [ SortableField.Firstname, SortOrder.Ascending ])
    in
    create
      Start.StartNow
      (Ptime_clock.now ()
       |> flip Ptime.add_span (Ptime.Span.of_int_s 3600)
       |> CCOption.get_exn_or "Invalid end time of mailing"
       |> EndAt.create
       |> get_or_failwith)
      rate
      distribution
    |> get_or_failwith
  in
  let%lwt () = Mailing.(Created (mailing, id) |> handle_event pool) in
  let create_message experiment =
    Message_template.ExperimentInvitation.prepare tenant experiment
  in
  Mailing.find_current pool
  ||> tap (CCList.length %> Alcotest.(check int "count mailings" 1))
  >|> Lwt_list.iter_s (fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
    let interval = 2 * 60 |> Ptime.Span.of_int_s in
    let%lwt experiment, contacts =
      Matcher.find_contacts_by_mailing
        pool
        mailing
        (Matcher.count_of_rate ~interval rate)
      ||> CCResult.get_exn
    in
    let%lwt create_message = create_message experiment in
    let expected = expected_events experiment contacts create_message in
    let%lwt events = Matcher.match_invitation_events ~interval [ pool ] in
    let events =
      CCList.assoc ~eq:Pool_database.Label.equal pool events |> sort_events
    in
    let () = Test_utils.check_result expected (Ok events) in
    let%lwt before = find_invitation_count experiment in
    let%lwt () = Pool_event.handle_events pool events in
    let%lwt after = find_invitation_count experiment in
    let () =
      let msg = "count generated invitations -> smaller or equal rate" in
      let is_less_or_equal = after - before <= Mailing.Rate.value rate in
      Alcotest.(check bool msg true is_less_or_equal)
    in
    Lwt.return_unit)
;;
