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
  let regex = "^.{4,}$"
  let error = Validation.Error.Invalid
  let admin_hint = "hint"
  let name = CCList.map (fun l -> l, "name") sys_languages
  let hint = CCList.map (fun l -> l, "hint") sys_languages

  let data =
    Pool_common.Message.
      [ Field.(Model |> show), model |> Model.show
      ; Field.(FieldType |> show), field_type |> FieldType.show
      ; Field.(Regex |> show), regex
      ; Field.(ErrorMessage |> show), error |> Validation.Error.show
      ; Field.(AdminHint |> show), admin_hint
      ]
    |> CCList.map (fun (f, l) -> f, l |> CCList.pure)
  ;;

  let custom_field =
    let get = CCResult.get_exn in
    let name = Name.create sys_languages name |> get in
    let hint = Hint.create hint |> get in
    let regex = Validation.Regex.create regex |> get in
    let validation = Validation.{ regex; error } in
    let admin_hint = Admin.Hint.create admin_hint |> get in
    let admin =
      Admin.{ hint = Some admin_hint; overwrite = Overwrite.create false }
    in
    { id
    ; model
    ; name
    ; hint
    ; field_type
    ; validation
    ; required = Required.create false
    ; disabled = Disabled.create false
    ; admin
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
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
