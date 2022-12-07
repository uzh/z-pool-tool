module Message = Pool_common.Message
module Field = Message.Field
module Language = Pool_common.Language

let database_label = Test_utils.Data.database_label

let save_custom_fields custom_field contact =
  let public = Custom_field_test.Data.to_public custom_field in
  let events =
    [ Custom_field.Created custom_field |> Pool_event.custom_field
    ; Custom_field.Published custom_field |> Pool_event.custom_field
    ; Custom_field.AnswerUpserted (public, Contact.id contact)
      |> Pool_event.custom_field
    ]
  in
  Lwt_list.iter_s (Pool_event.handle_event database_label) events
;;

let update_with_old_version _ () =
  let%lwt () =
    let open CCResult in
    let contact = Test_utils.Model.create_contact () in
    let language = Language.De in
    let contact =
      Contact.
        { contact with language_version = 1 |> Pool_common.Version.of_int }
    in
    let field = Message.Field.Language in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Contact.validate_partial_update
        contact
        database_label
        (field, version, [ Pool_common.Language.show language ], None)
    in
    let expected = Error Message.(MeantimeUpdate field) in
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
    let contact = Test_utils.Model.create_contact () in
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
        (field, version, [ new_value ], Some (Public.id public))
    in
    let expected =
      let[@warning "-4"] expected_field =
        match public with
        | Public.Text (p, answer) ->
          let answer =
            answer
            |> CCOption.map (fun a -> Answer.{ a with value = new_value })
          in
          Public.Text (p, answer)
        | _ -> failwith "Wrong field type"
      in
      Ok Contact.PartialUpdate.(Custom expected_field |> increment_version)
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

let partial_update_exec
  ?is_admin
  ?(custom_field = Custom_field_test.Data.custom_text_field ())
  ?(value = "testvalue")
  expected
  ()
  =
  let%lwt () =
    let open Custom_field in
    let contact = Test_utils.Model.create_contact () in
    let public = Custom_field_test.Data.to_public custom_field in
    let%lwt () = save_custom_fields custom_field contact in
    let language = Pool_common.Language.En in
    let field = Public.to_common_field language public in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Contact.validate_partial_update
        ?is_admin
        contact
        database_label
        (field, version, [ value ], Some (Public.id public))
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
  let validation = [ "text_length_max", "10" ] in
  let custom_field = Custom_field_test.Data.custom_text_field ~validation () in
  let value = "this value is longer than 10" in
  let expected = Error Message.(TextLengthMax 10) in
  partial_update_exec ~custom_field ~value expected ()
;;

let update_admin_input_only_field_as_user _ () =
  let open Custom_field in
  let admin =
    Admin.
      { Custom_field_test.Data.admin with
        input_only = true |> InputOnly.create
      }
  in
  let custom_field = Custom_field_test.Data.custom_text_field ~admin () in
  let expected = Error Message.NotEligible in
  partial_update_exec ~custom_field expected ()
;;

let update_non_overwrite_field_as_admin _ () =
  let open Custom_field in
  let admin =
    Admin.
      { Custom_field_test.Data.admin with
        overwrite = false |> Overwrite.create
      }
  in
  let custom_field = Custom_field_test.Data.custom_text_field ~admin () in
  let expected = Error Message.NotEligible in
  partial_update_exec ~is_admin:true ~custom_field expected ()
;;
