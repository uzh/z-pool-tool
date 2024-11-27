open Pool_message
module CustomFieldCommand = Cqrs_command.Custom_field_command
module CustomFieldOptionCommand = Cqrs_command.Custom_field_option_command

let boolean_fields = Custom_field.boolean_fields |> CCList.map Field.show

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

  let select_option name =
    let open CCResult in
    let lang = Pool_common.Language.En in
    Name.create [ Pool_common.Language.En ] [ lang, name ]
    >|= SelectOption.create
    |> get_exn
  ;;

  let select_option_to_public { SelectOption.id; name; _ } =
    SelectOption.Public.create ~id name
  ;;

  let custom_field
        ?published_at
        ?select_options
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
      ?select_options
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

  let custom_select_field ?select_options ?validation () =
    custom_field
      ?select_options
      ~validation:(CCOption.value ~default:[] validation)
      FieldType.Select
  ;;

  let custom_multi_select_field ?select_options ?validation () =
    custom_field
      ?select_options
      ~validation:(CCOption.value ~default:[] validation)
      FieldType.MultiSelect
  ;;

  let custom_number_field ?validation () = custom_field ?validation FieldType.Number
  let answer_id = Answer.Id.create ()

  let to_public ?(field_options = []) entity_uuid (m : Custom_field.t) =
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
        { Answer.id = answer_id; entity_uuid; value = Some true; admin_value }
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
        { Answer.id = answer_id; entity_uuid; value = Some (1970, 1, 1); admin_value }
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
        |> Answer.create ~id:answer_id entity_uuid
      in
      let validation = validation_schema Validation.MultiSelect.schema in
      Public.MultiSelect
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
        , field_options
        , Some answer )
    | FieldType.Number ->
      let answer =
        { Answer.id = answer_id; entity_uuid; value = Some 3; admin_value }
        |> CCOption.pure
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
        { Answer.id = answer_id; entity_uuid; value = option; admin_value }
        |> CCOption.pure
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
        { Answer.id = answer_id; entity_uuid; value = Some "test"; admin_value }
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
  let expected = Ok [ Custom_field.Created custom_field |> Pool_event.custom_field ] in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
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
  let expected = Error (Error.AllLanguagesRequired Field.Name) in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
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
    Ok [ Custom_field.Updated (custom_field, custom_field) |> Pool_event.custom_field ]
  in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let update_type_of_published_field () =
  let open CCResult in
  let custom_field =
    Data.custom_text_field ~published_at:(Custom_field.PublishedAt.create_now ()) ()
  in
  let events =
    let data =
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
  let expected = Error Error.CustomFieldTypeChangeNotAllowed in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let create_option () =
  let select_field = Data.custom_select_field () in
  let option_id = Custom_field.SelectOption.Id.create () in
  let name = Custom_field.Name.create Data.sys_languages Data.name |> CCResult.get_exn in
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
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let delete_published_field () =
  let custom_field =
    Data.custom_text_field ~published_at:(Custom_field.PublishedAt.create_now ()) ()
  in
  let events = Cqrs_command.Custom_field_command.Delete.handle custom_field in
  let expected = Error (Error.AlreadyPublished Field.CustomField) in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
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
  let expected = Error (Error.AlreadyPublished Field.CustomFieldOption) in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let publish_field_without_options () =
  let custom_field = Data.custom_select_field () in
  let events = Cqrs_command.Custom_field_command.Publish.handle custom_field in
  let expected = Error Error.CustomFieldNoOptions in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let publish_field_with_options () =
  let select_options = Data.select_option "name" |> CCList.return in
  let custom_field = Data.custom_select_field ~select_options () in
  let events = Cqrs_command.Custom_field_command.Publish.handle custom_field in
  let expected = Ok [ Custom_field.Published custom_field |> Pool_event.custom_field ] in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

module ValidationTests = struct
  open Custom_field

  let contact = Test_utils.Model.create_contact ()
  let contact_id = Contact.(contact |> id |> Id.to_common)

  let check_result expected generated =
    let open Alcotest in
    let field = testable Public.pp Public.equal in
    check (result field Test_utils.error) "succeeds" expected generated
  ;;

  let update_answer answer valid_value =
    answer
    |> CCOption.get_exn_or "No answer"
    |> fun a -> Answer.{ a with value = Some valid_value }
  ;;

  let validate_text_field () =
    let min_length = 2 in
    let max_length = 5 in
    let validation =
      Validation.Text.
        [ show_key TextLengthMin, CCInt.to_string min_length
        ; show_key TextLengthMax, CCInt.to_string max_length
        ]
    in
    let custom_field =
      Data.custom_text_field ~validation () |> Data.to_public contact_id
    in
    let validate value =
      validate_htmx ~is_admin:false ~entity_uuid:contact_id [ value ] custom_field
    in
    let validate_too_short () =
      let result = validate "x" in
      let expected = Error (Error.TextLengthMin min_length) in
      check_result expected result
    in
    let validate_too_long () =
      let result = validate "foobar" in
      let expected = Error (Error.TextLengthMax max_length) in
      check_result expected result
    in
    let validate_ok () =
      let valid_value = "foo" in
      let result = validate valid_value in
      let[@warning "-4"] expected =
        result
        |> Test_utils.get_or_failwith
        |> function
        | Public.Text (field, answer) ->
          let answer = update_answer answer valid_value in
          Ok (Public.Text (field, Some answer))
        | _ -> failwith "Invalid custom field type"
      in
      check_result expected result
    in
    let () = validate_too_short () in
    let () = validate_too_long () in
    let () = validate_ok () in
    ()
  ;;

  let validate_number_field () =
    let min_num = 2 in
    let max_num = 5 in
    let validation =
      Validation.Number.
        [ show_key NumberMin, CCInt.to_string min_num
        ; show_key NumberMax, CCInt.to_string max_num
        ]
    in
    let custom_field =
      Data.custom_number_field ~validation () |> Data.to_public contact_id
    in
    let validate value =
      validate_htmx
        ~is_admin:false
        ~entity_uuid:contact_id
        [ CCInt.to_string value ]
        custom_field
    in
    let validate_too_small () =
      let result = validate 1 in
      let expected = Error (Error.NumberMin min_num) in
      check_result expected result
    in
    let validate_too_big () =
      let result = validate 10 in
      let expected = Error (Error.NumberMax max_num) in
      check_result expected result
    in
    let validate_ok () =
      let valid_value = 3 in
      let result = validate valid_value in
      let[@warning "-4"] expected =
        result
        |> Test_utils.get_or_failwith
        |> function
        | Public.Number (field, answer) ->
          let answer = update_answer answer valid_value in
          Ok (Public.Number (field, Some answer))
        | _ -> failwith "Invalid custom field type"
      in
      check_result expected result
    in
    let () = validate_too_small () in
    let () = validate_too_big () in
    let () = validate_ok () in
    ()
  ;;

  let validate_multi_select_field () =
    let min_num = 2 in
    let max_num = 3 in
    let validation =
      Validation.MultiSelect.
        [ show_key OptionsCountMin, CCInt.to_string min_num
        ; show_key OptionsCountMax, CCInt.to_string max_num
        ]
    in
    let select_options =
      [ 1; 2; 3; 4; 5 ] |> CCList.map (fun i -> Data.select_option (CCInt.to_string i))
    in
    let public_options = select_options |> CCList.map Data.select_option_to_public in
    let custom_field =
      Data.custom_multi_select_field ~validation ~select_options ()
      |> Data.to_public ~field_options:public_options contact_id
    in
    let validate options =
      validate_htmx
        ~is_admin:false
        ~entity_uuid:contact_id
        (CCList.map SelectOption.(fun { Public.id; _ } -> Id.value id) options)
        custom_field
    in
    let validate_too_few () =
      let result = validate (CCList.take 1 public_options) in
      let expected = Error (Error.SelectedOptionsCountMin min_num) in
      check_result expected result
    in
    let validate_too_many () =
      let result = validate public_options in
      let expected = Error (Error.SelectedOptionsCountMax max_num) in
      check_result expected result
    in
    let validate_ok () =
      let valid_value = CCList.take 2 public_options in
      let result = validate valid_value in
      let[@warning "-4"] expected =
        result
        |> Test_utils.get_or_failwith
        |> function
        | Public.MultiSelect (field, options, answer) ->
          let answer = update_answer answer valid_value in
          Ok (Public.MultiSelect (field, options, Some answer))
        | _ -> failwith "Invalid custom field type"
      in
      check_result expected result
    in
    let () = validate_too_few () in
    let () = validate_too_many () in
    let () = validate_ok () in
    ()
  ;;
end

module Settings = struct
  let update_visibility _ =
    let open Custom_field in
    let field1 = Data.custom_text_field () |> set_show_on_session_close_page true in
    let field2 = Data.custom_text_field () in
    let field3 = Data.custom_text_field () in
    let selected = [ field2 |> id |> Id.value ] in
    let expected =
      (* Expect fields that have not been updated to be ignored *)
      let active = [ Updated (field2, field2 |> set_show_on_session_close_page true) ] in
      let inactive =
        [ Updated (field1, field1 |> set_show_on_session_close_page false) ]
      in
      active @ inactive |> Pool_event.(map custom_field) |> CCResult.return
    in
    let result =
      Cqrs_command.Custom_field_settings_command.UpdateVisibilitySettings.handle
        ~selected
        `Close
        [ field1; field2; field3 ]
        ()
    in
    Test_utils.check_result expected result
  ;;
end
