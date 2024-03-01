open Pool_message
module Command = Cqrs_command.Organisational_unit_command
module Language = Pool_common.Language

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let create_succeeds () =
  let open CCResult in
  let new_name = "SNS" in
  let new_id = Organisational_unit.Id.create () in
  let data = [ "name", [ new_name ] ] in
  let events = Command.(data |> decode >>= Create.handle ~id:new_id) in
  let expected =
    let open Organisational_unit in
    let ou = new_name |> Name.of_string |> create ~id:new_id in
    Ok [ Created ou |> Pool_event.organisational_unit ]
  in
  check_result expected events
;;

let create_fails () =
  let open CCResult in
  let name = "SNS" in
  let id = Organisational_unit.Id.create () in
  let data = [ name, [ "" ] ] in
  let events = Command.(data |> decode >>= Create.handle ~id) in
  let expected = Error Error.(Conformist [ Field.Name, NoValue ]) in
  check_result expected events
;;
