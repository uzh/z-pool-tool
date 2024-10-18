open Pool_version
module Command = Cqrs_command.Pool_version_command
open Pool_message

let create_version_tag () =
  let testable_version = Version.(Alcotest.testable pp equal) in
  let invalid_tags = [ ""; "invalid"; "1"; "1.1"; "1.1.1.1" ] in
  let valid_tags = [ "1.1.1"; "0.1.2"; "10.1.0" ] in
  let test msg expected tag =
    let res = Version.create tag in
    Alcotest.(check Test_utils.(result testable_version error) msg expected res)
  in
  let () =
    invalid_tags
    |> CCList.iter (test "invalid" (Error (Error.Invalid Field.Version)))
  in
  let () =
    valid_tags
    |> CCList.iter (fun tag ->
      let expected = Ok (Version.of_string tag) in
      test "valid" expected tag)
  in
  ()
;;

let create () =
  let version_field = Field.(show Version) in
  let text_field = Field.(show Text) in
  let id = Id.create () in
  let create = create ~id in
  let run_test urlencoded expected msg =
    let open CCResult in
    let result =
      let open Command.Create in
      urlencoded |> decode >>= handle ~id
    in
    Test_utils.check_result ~msg expected result
  in
  let () =
    let urlencoded = [ version_field, [ "1.1.1" ]; text_field, [ "text" ] ] in
    let expected = create (Version.of_string "1.1.1") (Text.of_string "text") in
    let expected = Ok [ Created expected |> Pool_event.pool_version ] in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ text_field, [ "text" ] ] in
    let expected = Error Error.(Conformist [ Field.Version, NoValue ]) in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ version_field, [ "1.0.0" ] ] in
    let expected = Error Error.(Conformist [ Field.Text, NoValue ]) in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ version_field, [ "1" ]; text_field, [ "text" ] ] in
    let expected =
      Error Error.(Conformist [ Field.Version, Invalid Field.Version ])
    in
    run_test urlencoded expected "create version"
  in
  ()
;;

let update () =
  let version_field = Field.(show Version) in
  let text_field = Field.(show Text) in
  let id = Id.create () in
  let version =
    Pool_version.create ~id (Version.of_string "1.1.1") (Text.of_string "text")
  in
  let run_test urlencoded expected msg =
    let open CCResult in
    let result =
      let open Command.Create in
      urlencoded |> decode >>= handle ~id
    in
    Test_utils.check_result ~msg expected result
  in
  let () =
    let urlencoded = [ text_field, [ "updated" ] ] in
    let text = Text.of_string "updated" in
    let expected =
      Ok [ Updated { version with text } |> Pool_event.pool_version ]
    in
    run_test urlencoded expected "update version"
  in
  let () =
    let urlencoded =
      [ version_field, [ "1.2.3" ]; text_field, [ "updated" ] ]
    in
    let text = Text.of_string "updated" in
    let expected =
      Ok [ Updated { version with text } |> Pool_event.pool_version ]
    in
    run_test urlencoded expected "update version"
  in
  ()
;;
