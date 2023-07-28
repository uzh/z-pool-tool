module CustomFieldCommand = Cqrs_command.Custom_field_command
module CustomFieldOptionCommand = Cqrs_command.Custom_field_option_command
module Message = Pool_common.Message

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Message.Field.show
;;

module Data = struct
  open Custom_field

  let get = CCResult.get_exn
  let sys_languages = Pool_common.Language.[ En; De ]
  let model = Model.Contact
  let field_type = FieldType.Text
  let admin_hint = "hint"
  let name = CCList.map (fun l -> l, "name") sys_languages
  let hint = CCList.map (fun l -> l, "hint") sys_languages
  let validation_data = [ "text_length_max", "20" ]
  let disabled = false |> Disabled.create
  let required = false |> Required.create

  let data =
    Message.
      [ Field.(FieldType |> show), field_type |> FieldType.show
      ; Field.(AdminHint |> show), admin_hint
      ]
    |> CCList.map (fun (f, l) -> f, l |> CCList.pure)
  ;;

  let admin_hint = AdminHint.create admin_hint |> get |> CCOption.pure
  let admin_override = AdminOverride.create false
  let admin_view_only = AdminViewOnly.create false
  let admin_input_only = AdminInputOnly.create false
  let prompt_on_registration = PromptOnRegistration.create false

  let custom_field
    ?published_at
    ?validation
    ?(required = required)
    ?(admin_override = AdminOverride.create false)
    ?(admin_input_only = AdminInputOnly.create false)
    ?(prompt_on_registration = PromptOnRegistration.create false)
    field_type
    =
    let name = Name.create sys_languages name |> get in
    let hint = Hint.create hint |> get in
    let admin_view_only = AdminViewOnly.create false in
    Custom_field.create
      ~id:(Id.create ())
      ?published_at
      field_type
      model
      name
      hint
      (validation |> CCOption.value ~default:validation_data)
      required
      disabled
      None
      admin_hint
      admin_override
      admin_view_only
      admin_input_only
      prompt_on_registration
    |> CCResult.get_exn
  ;;

  let custom_text_field
    ?published_at
    ?validation
    ?admin_override
    ?admin_input_only
    ?required
    ()
    =
    custom_field
      ?published_at
      ?validation
      ?admin_override
      ?admin_input_only
      ?required
      FieldType.Text
  ;;

  let custom_select_field () = custom_field ~validation:[] FieldType.Select

  let custom_number_field ?validation () =
    custom_field ?validation FieldType.Number
  ;;

  let answer_id = Answer.Id.create ()

  let to_public ?(field_options = []) (m : Custom_field.t) =
    let open Custom_field in
    let validation_schema schema =
      let validation = validation_to_yojson m in
      Custom_field.(Validation.(validation |> raw_of_yojson |> schema))
    in
    let field_type = field_type m in
    let id = id m in
    let hint = hint m in
    let name = name m in
    let required = required m in
    let admin_override = admin_override m in
    let admin_input_only = admin_input_only m in
    let prompt_on_registration = prompt_on_registration m in
    let version = 0 |> Pool_common.Version.of_int in
    let admin_value = None in
    match field_type with
    | FieldType.Boolean ->
      let answer =
        { Answer.id = answer_id; value = Some true; admin_value }
        |> CCOption.pure
      in
      Public.Boolean
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , answer )
    | FieldType.Date ->
      let answer =
        { Answer.id = answer_id; value = Some (1970, 1, 1); admin_value }
        |> CCOption.pure
      in
      Public.Date
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , answer )
    | FieldType.MultiSelect ->
      let answer =
        field_options
        |> CCList.head_opt
        |> CCOption.map CCList.pure
        |> Answer.create ~id:answer_id
      in
      Public.MultiSelect
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , field_options
        , Some answer )
    | FieldType.Number ->
      let answer =
        { Answer.id = answer_id; value = Some 3; admin_value } |> CCOption.pure
      in
      let validation = validation_schema Validation.Number.schema in
      Public.Number
        ( { Public.id
          ; name
          ; hint
          ; validation
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , answer )
    | FieldType.Select ->
      let answer =
        CCList.head_opt field_options
        |> fun option ->
        { Answer.id = answer_id; value = option; admin_value } |> CCOption.pure
      in
      Public.Select
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , field_options
        , answer )
    | FieldType.Text ->
      let answer =
        { Answer.id = answer_id; value = Some "test"; admin_value }
        |> CCOption.pure
      in
      let validation = validation_schema Validation.Text.schema in
      Public.Text
        ( { Public.id
          ; name
          ; hint
          ; validation
          ; required
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , answer )
  ;;
end

let database_label = Test_utils.Data.database_label

let create () =
  let open CCResult in
  let custom_field = Data.custom_text_field () in
  let id = Custom_field.id custom_field in
  let events =
    Data.data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Create.handle
          ~id
          Data.sys_languages
          Data.model
          Data.name
          Data.hint
          Data.validation_data
  in
  let expected =
    Ok [ Custom_field.Created custom_field |> Pool_event.custom_field ]
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
          ~id:(Custom_field.Id.create ())
          Data.sys_languages
          Data.model
          (Data.name |> CCList.hd |> CCList.pure)
          Data.hint
          Data.validation_data
  in
  let expected = Error Message.(AllLanguagesRequired Field.Name) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let update () =
  let open CCResult in
  let custom_field = Data.custom_text_field () in
  let events =
    Data.data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Update.handle
          Data.sys_languages
          custom_field
          Data.name
          Data.hint
          Data.validation_data
  in
  let expected =
    Ok [ Custom_field.Updated custom_field |> Pool_event.custom_field ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let update_type_of_published_field () =
  let open CCResult in
  let custom_field =
    Data.custom_text_field
      ~published_at:(Custom_field.PublishedAt.create_now ())
      ()
  in
  let events =
    let data =
      Message.
        [ Field.(FieldType |> show), [ Custom_field.FieldType.(Number |> show) ]
        ; Field.(AdminHint |> show), [ "hint" ]
        ]
    in
    data
    |> Http_utils.format_request_boolean_values boolean_fields
    |> CustomFieldCommand.base_decode
    >>= CustomFieldCommand.Update.handle
          Data.sys_languages
          custom_field
          Data.name
          Data.hint
          Data.validation_data
  in
  let expected = Error Pool_common.Message.CustomFieldTypeChangeNotAllowed in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let create_option () =
  let select_field = Data.custom_select_field () in
  let option_id = Custom_field.SelectOption.Id.create () in
  let name =
    Custom_field.Name.create Data.sys_languages Data.name |> CCResult.get_exn
  in
  let option = Custom_field.SelectOption.create ~id:option_id name in
  let events =
    CustomFieldOptionCommand.Create.handle
      ~id:option_id
      Data.sys_languages
      select_field
      Data.name
  in
  let expected =
    Ok
      [ Custom_field.OptionCreated (Custom_field.id select_field, option)
        |> Pool_event.custom_field
      ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let delete_published_field () =
  let custom_field =
    Data.custom_text_field
      ~published_at:(Custom_field.PublishedAt.create_now ())
      ()
  in
  let events = Cqrs_command.Custom_field_command.Delete.handle custom_field in
  let expected =
    Error Pool_common.Message.(AlreadyPublished Field.CustomField)
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let delete_published_option () =
  let open Custom_field in
  let custom_field_option =
    let name = Data.(Name.create sys_languages name |> get) in
    SelectOption.create ~published_at:(PublishedAt.create_now ()) name
  in
  let events =
    Cqrs_command.Custom_field_option_command.Destroy.handle custom_field_option
  in
  let expected =
    Error Pool_common.Message.(AlreadyPublished Field.CustomFieldOption)
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
