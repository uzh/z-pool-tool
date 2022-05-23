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
  let contact = Test_utils.create_contact () in
  let events =
    let open WaitingListCommand in
    let command = { experiment; contact } in
    Create.handle command
  in
  let expected =
    Ok
      [ Waiting_list.(Created { experiment; contact })
        |> Pool_event.waiting_list
      ]
  in
  check_result expected events
;;

let delete () =
  let open Waiting_list in
  let experiment = Test_utils.create_public_experiment () in
  let contact = Test_utils.create_contact () in
  let events =
    let open WaitingListCommand in
    let command = { experiment; contact } in
    Destroy.handle command
  in
  let expected =
    Ok
      [ Waiting_list.(Deleted { experiment; contact })
        |> Pool_event.waiting_list
      ]
  in
  check_result expected events
;;

let create_with_waiting_list_disabled () =
  let open Waiting_list in
  let experiment = Test_utils.create_public_experiment () in
  let experiment =
    Experiment.Public.
      { experiment with
        waiting_list_disabled = true |> Experiment.WaitingListDisabled.create
      }
  in
  let contact = Test_utils.create_contact () in
  let events =
    let open WaitingListCommand in
    let command = { experiment; contact } in
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
