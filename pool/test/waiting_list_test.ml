module WaitingListCommand = Cqrs_command.Waiting_list_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let create () =
  let open Waiting_list in
  let experiment = Model.create_public_experiment () in
  let experiment =
    Experiment.Public.
      { experiment with
        direct_registration_disabled =
          true |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let contact = Model.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle command
  in
  let expected =
    Ok [ Waiting_list.Created command |> Pool_event.waiting_list ]
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
  let experiment =
    Experiment.Public.
      { experiment with
        direct_registration_disabled =
          false |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let contact = Model.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle command
  in
  let expected = Error Pool_common.Message.NotEligible in
  Test_utils.check_result expected events
;;

let update () =
  let waiting_list = Model.create_waiting_list () in
  let urlencoded =
    [ Pool_common.Message.Field.(AdminComment |> show), [ "Some comment" ] ]
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
  let contact_id = Pool_common.Id.create ()
  let experiment_id = Experiment.Id.create ()

  let find_pending_waitinglists_by_contact _ () =
    let open Utils.Lwt_result.Infix in
    let open Integration_utils in
    let%lwt contact =
      ContactRepo.create ~id:contact_id ~with_terms_accepted:true ()
    in
    let%lwt experiment =
      ExperimentRepo.create ~id:experiment_id ()
      ||> Model.experiment_to_public_experiment
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
      ||> CCList.find_opt (Experiment.Public.equal experiment)
          %> CCOption.is_some
    in
    let () = Alcotest.(check bool "succeeds" true res) in
    Lwt.return_unit
  ;;
end
