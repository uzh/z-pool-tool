module WaitingListCommand = Cqrs_command.Waiting_list_command
module Field = Pool_message.Field
module Model = Test_utils.Model

let get_exn = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label

let current_user () =
  Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
;;

let create () =
  let open Waiting_list in
  let experiment = Model.create_public_experiment () in
  let confirmation =
    Test_utils.Model.create_email_job
      ?smtp_auth_id:(Experiment.Public.smtp_auth_id experiment)
      ()
  in
  let experiment =
    Experiment.(
      true
      |> DirectRegistrationDisabled.create
      |> Public.update_direct_registration_disabled experiment)
  in
  let contact = Model.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle confirmation command
  in
  let expected =
    Ok
      [ Waiting_list.Created command |> Pool_event.waiting_list
      ; Email.sent confirmation |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;

let delete () =
  let waiting_list = Model.create_waiting_list () in
  let events =
    let open WaitingListCommand in
    Destroy.handle waiting_list
  in
  let expected =
    Ok [ Waiting_list.(Deleted waiting_list) |> Pool_event.waiting_list ]
  in
  Test_utils.check_result expected events
;;

let create_with_direct_registration_enabled () =
  let open Waiting_list in
  let experiment = Model.create_public_experiment () in
  let confirmation =
    Test_utils.Model.create_email_job
      ?smtp_auth_id:(Experiment.Public.smtp_auth_id experiment)
      ()
  in
  let experiment =
    Experiment.(
      false
      |> DirectRegistrationDisabled.create
      |> Public.update_direct_registration_disabled experiment)
  in
  let contact = Model.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle confirmation command
  in
  let expected = Error Pool_message.Error.NotEligible in
  Test_utils.check_result expected events
;;

let update () =
  let waiting_list = Model.create_waiting_list () in
  let urlencoded =
    [ Pool_message.Field.(AdminComment |> show), [ "Some comment" ] ]
  in
  let events =
    let open CCResult in
    let open WaitingListCommand in
    urlencoded |> Update.decode >>= Update.handle waiting_list
  in
  let expected =
    let open CCResult in
    let* decoded = urlencoded |> WaitingListCommand.Update.decode in
    Ok
      [ Waiting_list.(Updated (decoded, waiting_list))
        |> Pool_event.waiting_list
      ]
  in
  Test_utils.check_result expected events
;;

(* Integration *)

module PendingWaitingLists = struct
  let contact_id = Contact.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let session_id = Session.Id.create ()

  let find_pending_waitinglists_by_contact _ () =
    let open Utils.Lwt_result.Infix in
    let open Integration_utils in
    let%lwt contact =
      ContactRepo.create ~id:contact_id ~with_terms_accepted:true ()
    in
    let%lwt experiment =
      ExperimentRepo.create ~id:experiment_id () ||> Experiment.to_public
    in
    let%lwt (_ : Waiting_list.t) =
      WaitingListRepo.create experiment contact ()
      ||> CCOption.get_exn_or "Waiting list not found"
    in
    let%lwt res =
      let open CCFun in
      Experiment.find_pending_waitinglists_by_contact
        Test_utils.Data.database_label
        contact
      |> Lwt.map
           (CCList.find_opt (Experiment.Public.equal experiment)
            %> CCOption.is_some)
    in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;

  let exclude_after_assignign_to_session _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt experiment =
      Experiment.find database_label experiment_id ||> get_exn
    in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session =
      Integration_utils.SessionRepo.create ~id:session_id experiment ()
    in
    let%lwt (_ : Assignment.t) =
      Integration_utils.AssignmentRepo.create session contact
    in
    let%lwt res =
      let open CCFun in
      let open Experiment in
      find_pending_waitinglists_by_contact
        Test_utils.Data.database_label
        contact
      |> Lwt.map
           (CCList.find_opt (fun public ->
              Id.equal (Public.id public) experiment_id)
            %> CCOption.is_none)
    in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;

  let include_after_session_cancellation _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt current_user = current_user () in
    let%lwt experiment =
      Experiment.find database_label experiment_id
      ||> get_exn
      ||> Experiment.to_public
    in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt session = Session.find database_label session_id ||> get_exn in
    let%lwt () =
      Session.Canceled session
      |> Pool_event.session
      |> Pool_event.handle_event database_label current_user
    in
    let%lwt res =
      let open CCFun in
      Experiment.find_pending_waitinglists_by_contact
        Test_utils.Data.database_label
        contact
      |> Lwt.map
           (CCList.find_opt (Experiment.Public.equal experiment)
            %> CCOption.is_some)
    in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;
end
