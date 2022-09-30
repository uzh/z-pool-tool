module Message = Pool_common.Message
module Field = Message.Field
module Language = Pool_common.Language

let database_label = Test_utils.Data.database_label

let save_custom_fields custom_field contact =
  let public = Custom_field_test.Data.to_public custom_field in
  let events =
    [ Custom_field.Created custom_field |> Pool_event.custom_field
    ; Custom_field.AnswerUpserted (public, Contact.id contact)
      |> Pool_event.custom_field
    ]
  in
  Lwt_list.iter_s (Pool_event.handle_event database_label) events
;;

let update_with_old_version _ () =
  let%lwt () =
    let open CCResult in
    let contact = Test_utils.create_contact () in
    let language = Language.De in
    let contact =
      Contact.
        { contact with language_version = 1 |> Pool_common.Version.of_int }
    in
    let field = Pool_common.Message.Field.Language in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Contact.validate_partial_update
        contact
        database_label
        (field, version, Pool_common.Language.show language, None)
    in
    let expected = Error Pool_common.Message.(MeantimeUpdate field) in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let update_custom_field _ () =
  let%lwt () =
    let open CCResult in
    let open Custom_field in
    let contact = Test_utils.create_contact () in
    let custom_field = Custom_field_test.Data.custom_text_field () in
    let public = Custom_field_test.Data.to_public custom_field in
    let%lwt () = save_custom_fields custom_field contact in
    let language = Pool_common.Language.En in
    let field = Public.to_common_field language public in
    let new_value = "new value" in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Contact.validate_partial_update
        contact
        database_label
        (field, version, new_value, Some (Public.get_id public))
    in
    let expected =
      let[@warning "-4"] expected_field =
        let open Public in
        match public with
        | Public.Text p ->
          let answer =
            p.answer
            |> CCOption.map (fun a -> Answer.{ a with value = new_value })
          in
          Public.Text { p with answer }
        | _ -> failwith "Wrong field type"
      in
      Ok Contact.PartialUpdate.(Custom expected_field)
    in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let update_custom_field_with_invalid_answer _ () =
  let%lwt () =
    let open CCResult in
    let open Custom_field in
    let contact = Test_utils.create_contact () in
    let validation = [ "text_length_max", "10" ] in
    let custom_field =
      Custom_field_test.Data.custom_text_field ~validation ()
    in
    let public = Custom_field_test.Data.to_public custom_field in
    let%lwt () = save_custom_fields custom_field contact in
    let language = Pool_common.Language.En in
    let field = Public.to_common_field language public in
    let new_value = "this value is longer than 10" in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Contact.validate_partial_update
        contact
        database_label
        (field, version, new_value, Some (Public.get_id public))
    in
    let expected = Error Pool_common.Message.(TextLengthMax 10) in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;
