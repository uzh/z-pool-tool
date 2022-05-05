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
  let subject = Test_utils.create_subject () in
  let events =
    let open WaitingListCommand in
    let command = { experiment; subject } in
    Create.handle command
  in
  let expected =
    Ok
      [ Waiting_list.(Created { experiment; subject })
        |> Pool_event.waiting_list
      ]
  in
  check_result expected events
;;

let delete () =
  let open Waiting_list in
  let experiment = Test_utils.create_public_experiment () in
  let subject = Test_utils.create_subject () in
  let events =
    let open WaitingListCommand in
    let command = { experiment; subject } in
    Destroy.handle command
  in
  let expected =
    Ok
      [ Waiting_list.(Deleted { experiment; subject })
        |> Pool_event.waiting_list
      ]
  in
  check_result expected events
;;
