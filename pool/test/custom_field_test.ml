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

  let admin =
    let admin_hint = Admin.Hint.create admin_hint |> get in
    Admin.
      { hint = Some admin_hint
      ; overwrite = Overwrite.create false
      ; view_only = ViewOnly.create false
      ; input_only = InputOnly.create false
      }
  ;;

  let custom_field
    ?published_at
    ?validation
    ?(admin = admin)
    ?(required = required)
    field_type
    =
    let name = Name.create sys_languages name |> get in
    let hint = Hint.create hint |> get in
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
      admin
    |> CCResult.get_exn
  ;;

  let custom_text_field ?published_at ?validation ?admin ?required () =
    custom_field ?published_at ?validation ?admin ?required FieldType.Text
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
    let admin_overwrite = (admin m).Admin.overwrite in
    let admin_input_only = (admin m).Admin.input_only in
    let version = 0 |> Pool_common.Version.of_int in
    match field_type with
    | FieldType.Boolean ->
      let answer = Answer.{ id = answer_id; value = true } |> CCOption.pure in
      Public.Boolean
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_overwrite
          ; admin_input_only
          ; version
          }
        , answer )
    | FieldType.MultiSelect ->
      let answer =
        field_options
        |> CCList.head_opt
        |> CCOption.map (fun opt ->
             opt |> CCList.pure |> Answer.create ~id:answer_id)
      in
      Public.MultiSelect
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_overwrite
          ; admin_input_only
          ; version
          }
        , field_options
        , answer )
    | FieldType.Number ->
      let answer = Answer.{ id = answer_id; value = 3 } |> CCOption.pure in
      let validation = validation_schema Validation.Number.schema in
      Public.Number
        ( { Public.id
          ; name
          ; hint
          ; validation
          ; required
          ; admin_overwrite
          ; admin_input_only
          ; version
          }
        , answer )
    | FieldType.Select ->
      let answer =
        CCList.head_opt field_options
        |> CCOption.map (fun option ->
             Answer.{ id = answer_id; value = option })
      in
      Public.Select
        ( { Public.id
          ; name
          ; hint
          ; validation = Validation.pure
          ; required
          ; admin_overwrite
          ; admin_input_only
          ; version
          }
        , field_options
        , answer )
    | FieldType.Text ->
      let answer = Answer.{ id = answer_id; value = "test" } |> CCOption.pure in
      let validation = validation_schema Validation.Text.schema in
      Public.Text
        ( { Public.id
          ; name
          ; hint
          ; validation
          ; required
          ; admin_overwrite
          ; admin_input_only
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
        ; Field.(AdminHint |> show), [ Data.admin_hint ]
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

let create_with_missing_admin_option () =
  let open Custom_field.Admin in
  let overwrite = true |> Overwrite.create in
  let view_only = true |> ViewOnly.create in
  let hint = None in
  let admin =
    let input_only = false |> InputOnly.create in
    create hint overwrite view_only input_only
  in
  let expected =
    let input_only = true |> InputOnly.create in
    create hint overwrite view_only input_only
  in
  Alcotest.(
    check
      (result Test_utils.custom_field_admin Test_utils.error)
      "succeeds"
      admin
      expected)
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
