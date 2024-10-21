open Pool_version
module Command = Cqrs_command.Pool_version_command
open Pool_message

let create_version_tag () =
  let testable_tag = Tag.(Alcotest.testable pp equal) in
  let invalid_tags = [ ""; "invalid"; "a.b.c"; "1"; "1.1"; "1.1.1.1" ] in
  let valid_tags = [ "1.1.1"; "0.1.2"; "10.1.0" ] in
  let test msg expected tag =
    let res = Tag.create tag in
    Alcotest.(check Test_utils.(result testable_tag error) msg expected res)
  in
  let () =
    invalid_tags
    |> CCList.iter (test "invalid" (Error (Error.Invalid Field.Tag)))
  in
  let () =
    valid_tags
    |> CCList.iter (fun tag ->
      let expected = Ok (Tag.of_string tag) in
      test "valid" expected tag)
  in
  ()
;;

let create () =
  let tag_field = Field.(show Tag) in
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
    let urlencoded = [ tag_field, [ "1.1.1" ]; text_field, [ "text" ] ] in
    let expected = create (Tag.of_string "1.1.1") (Text.of_string "text") in
    let expected = Ok [ Created expected |> Pool_event.pool_version ] in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ text_field, [ "text" ] ] in
    let expected = Error Error.(Conformist [ Field.Tag, NoValue ]) in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ tag_field, [ "1.0.0" ] ] in
    let expected = Error Error.(Conformist [ Field.Text, NoValue ]) in
    run_test urlencoded expected "create version"
  in
  let () =
    let urlencoded = [ tag_field, [ "1" ]; text_field, [ "text" ] ] in
    let expected = Error Error.(Conformist [ Field.Tag, Invalid Field.Tag ]) in
    run_test urlencoded expected "create version"
  in
  ()
;;

let update () =
  let tag_field = Field.(show Tag) in
  let text_field = Field.(show Text) in
  let id = Id.create () in
  let version =
    Pool_version.create ~id (Tag.of_string "1.1.1") (Text.of_string "text")
  in
  let run_test urlencoded expected msg =
    let open CCResult in
    let result =
      let open Command.Update in
      urlencoded |> decode >>= handle version
    in
    Test_utils.check_result ~msg expected result
  in
  let () =
    let urlencoded = [ text_field, [ "updated" ] ] in
    let text = Text.of_string "updated" in
    let expected =
      Ok [ Updated { version with text } |> Pool_event.pool_version ]
    in
    run_test urlencoded expected "update version valid"
  in
  let () =
    let urlencoded = [ tag_field, [ "1.2.3" ]; text_field, [ "updated" ] ] in
    let text = Text.of_string "updated" in
    let expected =
      Ok [ Updated { version with text } |> Pool_event.pool_version ]
    in
    run_test urlencoded expected "update version valid with tag to ignore"
  in
  ()
;;

let publish () =
  let announcement_id = Announcement.Id.create () in
  let version =
    Pool_version.create (Tag.of_string "1.1.1") (Text.of_string "text")
  in
  let tenant_ids = [ Pool_tenant.Id.create () ] in
  let run_test version expected msg =
    let result =
      let open Command.Publish in
      handle ~announcement_id tenant_ids version
    in
    Test_utils.check_result ~msg expected result
  in
  let () =
    let expected =
      let announcement =
        Pool_version.announcement ~id:announcement_id version
        |> Test_utils.get_or_failwith
      in
      Ok
        [ Published version |> Pool_event.pool_version
        ; Announcement.Created (announcement, tenant_ids)
          |> Pool_event.announcement
        ]
    in
    run_test version expected "publish version succeeds"
  in
  let () =
    let expected = Error Pool_message.(Error.AlreadyPublished Field.Version) in
    let version =
      Pool_version.
        { version with
          published_at = Some (PublishedAt.create (Ptime_clock.now ()))
        }
    in
    run_test version expected "pushish published version fails"
  in
  ()
;;
