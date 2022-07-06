module WaitingListCommand = Cqrs_command.Waiting_list_command
module Field = Pool_common.Message.Field

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let create () =
  let open Waiting_list in
  let experiment = Test_utils.create_public_experiment () in
  let experiment =
    Experiment.Public.
      { experiment with
        direct_registration_disabled =
          true |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let contact = Test_utils.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle command
  in
  let expected =
    Ok [ Waiting_list.Created command |> Pool_event.waiting_list ]
  in
  check_result expected events
;;

let delete () =
  let waiting_list = Test_utils.create_waiting_list () in
  let events =
    let open WaitingListCommand in
    Destroy.handle waiting_list
  in
  let expected =
    Ok [ Waiting_list.(Deleted waiting_list) |> Pool_event.waiting_list ]
  in
  check_result expected events
;;

let create_with_direct_registration_enabled () =
  let open Waiting_list in
  let experiment = Test_utils.create_public_experiment () in
  let experiment =
    Experiment.Public.
      { experiment with
        direct_registration_disabled =
          false |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let contact = Test_utils.create_contact () in
  let command = { experiment; contact } in
  let events =
    let open WaitingListCommand in
    Create.handle command
  in
  let expected = Error Pool_common.Message.NotEligible in
  check_result expected events
;;

let update () =
  let waiting_list = Test_utils.create_waiting_list () in
  let urlencoded =
    [ Pool_common.Message.Field.(Comment |> show), [ "Some comment" ] ]
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
  check_result expected events
;;
