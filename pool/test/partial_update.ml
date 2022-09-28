module Message = Pool_common.Message
module Field = Message.Field
module Language = Pool_common.Language

let database_label = Test_utils.Data.database_label

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
