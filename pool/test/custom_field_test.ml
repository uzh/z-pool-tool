module CustomFieldCommand = Cqrs_command.Custom_field_command

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

module Data = struct
  open Custom_field

  let sys_languages = Pool_common.Language.[ En; De ]
  let id = Id.create ()
  let model = Model.Contact
  let field_type = FieldType.Text
  let admin_hint = "hint"
  let name = CCList.map (fun l -> l, "name") sys_languages
  let hint = CCList.map (fun l -> l, "hint") sys_languages
  let validation_data = [ "text_length_max", "20" ]
  let disabled = false |> Disabled.create
  let required = false |> Required.create

  let data =
    Pool_common.Message.
      [ Field.(Model |> show), model |> Model.show
      ; Field.(FieldType |> show), field_type |> FieldType.show
      ; Field.(AdminHint |> show), admin_hint
      ]
    |> CCList.map (fun (f, l) -> f, l |> CCList.pure)
  ;;

  let custom_field =
    let get = CCResult.get_exn in
    let name = Name.create sys_languages name |> get in
    let hint = Hint.create hint |> get in
    let admin_hint = Admin.Hint.create admin_hint |> get in
    let admin =
      Admin.{ hint = Some admin_hint; overwrite = Overwrite.create false }
    in
    Custom_field.create
      ~id
      field_type
      model
      name
      hint
      validation_data
      required
      disabled
      admin
    |> CCResult.get_exn
  ;;
end

let database_label = Test_utils.Data.database_label

let create () =
  let open CCResult in
  let events =
    Data.data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Create.handle
          ~id:Data.id
          Data.sys_languages
          Data.name
          Data.hint
          Data.validation_data
  in
  let expected =
    Ok [ Custom_field.Created Data.custom_field |> Pool_event.custom_field ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let create_with_missing_name () =
  let open CCResult in
  let events =
    Data.data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Create.handle
          ~id:Data.id
          Data.sys_languages
          (Data.name |> CCList.hd |> CCList.pure)
          Data.hint
          Data.validation_data
  in
  let expected = Error Pool_common.Message.(AllLanguagesRequired Field.Name) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let update () =
  let open CCResult in
  let events =
    Data.data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Update.handle
          Data.sys_languages
          Data.custom_field
          Data.name
          Data.hint
          Data.validation_data
  in
  let expected =
    Ok [ Custom_field.Updated Data.custom_field |> Pool_event.custom_field ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
