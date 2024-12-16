open Pool_message

let get_exn = Test_utils.get_or_failwith
let lang = Pool_common.Language.En

module Data = struct
  open Custom_field

  let published = () |> PublishedAt.create_now |> CCOption.pure
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
    let field ?(validation = Validation.pure) () =
      { Public.id
      ; name
      ; hint
      ; validation
      ; required
      ; admin_override
      ; admin_input_only
      ; prompt_on_registration
      ; version
      }
    in
    match field_type with
    | FieldType.Boolean ->
      let answer =
        { Answer.id = answer_id; entity_uuid; value = Some true; admin_value }
        |> CCOption.pure
      in
      Public.Boolean (field (), answer)
    | FieldType.Date ->
      let answer =
        { Answer.id = answer_id; entity_uuid; value = Some (1970, 1, 1); admin_value }
        |> CCOption.pure
      in
      Public.Date (field (), answer)
    | FieldType.MultiSelect ->
      let answer =
        field_options
        |> CCList.head_opt
        |> CCOption.map CCList.pure
        |> Answer.create ~id:answer_id entity_uuid
      in
      let validation = validation_schema Validation.MultiSelect.schema in
      Public.MultiSelect (field ~validation (), field_options, Some answer)
    | FieldType.Number ->
      let answer =
        { Answer.id = answer_id; entity_uuid; value = Some 3; admin_value }
        |> CCOption.pure
      in
      let validation = validation_schema Validation.Number.schema in
      Public.Number (field ~validation (), answer)
    | FieldType.Select ->
      let answer =
        CCList.head_opt field_options
        |> fun option ->
        { Answer.id = answer_id; entity_uuid; value = option; admin_value }
        |> CCOption.pure
      in
      Public.Select (field (), field_options, answer)
    | FieldType.Text ->
      let answer =
        { Answer.id = answer_id; entity_uuid; value = Some "test"; admin_value }
        |> CCOption.pure
      in
      let validation = validation_schema Validation.Text.schema in
      Public.Text (field ~validation (), answer)
  ;;
end

let create_custom_field field_name encoder =
  Custom_field.
    { id = Id.create ()
    ; model = Model.Contact
    ; name = Name.create [ lang ] [ lang, field_name ] |> get_exn
    ; hint = [] |> Hint.create |> get_exn
    ; validation = Validation.pure
    ; required = false |> Required.create
    ; disabled = false |> Disabled.create
    ; custom_field_group_id = None
    ; admin_hint = Data.admin_hint
    ; admin_override = Data.admin_override
    ; admin_view_only = Data.admin_view_only
    ; admin_input_only = Data.admin_input_only
    ; published_at = Data.published
    ; prompt_on_registration = false |> PromptOnRegistration.create
    ; show_on_session_close_page = false
    ; show_on_session_detail_page = false
    ; duplicate_weighting = None
    }
  |> encoder
;;

let save_custom_field t = Custom_field.Created t |> Pool_event.custom_field

let save_options field =
  let field_id = Custom_field.id field in
  CCList.map (fun option ->
    Custom_field.OptionCreated (field_id, option) |> Pool_event.custom_field)
;;

module NrOfSiblings = struct
  let answer_value = 3
  let field = create_custom_field "Nr of siblings" (fun a -> Custom_field.Number a)

  let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer_value =
    let open Custom_field in
    let answer =
      match is_admin with
      | true -> Answer.create ?admin_value:answer_value entity_uuid None
      | false -> Answer.create entity_uuid answer_value
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.Number
      ( { Public.id = id field
        ; name = name field
        ; hint = hint field
        ; validation = Validation.pure
        ; required = required field
        ; admin_override = Data.admin_override
        ; admin_input_only = Data.admin_input_only
        ; prompt_on_registration = Data.prompt_on_registration
        ; version
        }
      , Some answer )
  ;;

  let save () = save_custom_field field

  let save_answers ~answer_value ?admin contacts =
    CCList.map
      (fun contact ->
         let user = admin |> CCOption.value ~default:(Pool_context.Contact contact) in
         Custom_field.AnswerUpserted
           (public (CCOption.is_some admin) answer_value, Contact.id contact, user)
         |> Pool_event.custom_field)
      contacts
  ;;
end

module Birthday = struct
  let answer_value = "1990-01-01" |> Pool_model.Base.Ptime.date_of_string |> get_exn
  let field = create_custom_field "Birthday" (fun a -> Custom_field.Date a)

  let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer_value =
    let open Custom_field in
    let answer =
      match is_admin with
      | true -> Answer.create ?admin_value:answer_value entity_uuid None
      | false -> Answer.create entity_uuid answer_value
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.Date
      ( { Public.id = id field
        ; name = name field
        ; hint = hint field
        ; validation = Validation.pure
        ; required = required field
        ; admin_override = Data.admin_override
        ; admin_input_only = Data.admin_input_only
        ; prompt_on_registration = Data.prompt_on_registration
        ; version
        }
      , Some answer )
  ;;

  let save () = save_custom_field field

  let save_answers ~answer_value ?admin contacts =
    CCList.map
      (fun contact ->
         let user = admin |> CCOption.value ~default:(Pool_context.Contact contact) in
         Custom_field.AnswerUpserted
           (public (CCOption.is_some admin) answer_value, Contact.id contact, user)
         |> Pool_event.custom_field)
      contacts
  ;;

  let filter ?date operator () =
    let open Filter in
    let value = date |> CCOption.value ~default:answer_value in
    let query =
      Pred
        (Predicate.create
           Key.(CustomField (field |> Custom_field.id))
           operator
           (Single (Date value)))
    in
    create None query
  ;;
end

module SelectField = struct
  let option_to_public =
    let open Custom_field in
    fun { SelectOption.id; name; _ } -> SelectOption.Public.create ~id name
  ;;

  let options =
    [ "1"; "2" ]
    |> CCList.map
         Custom_field.(
           fun label ->
             [ lang, label ] |> Name.create [ lang ] |> get_exn |> SelectOption.create)
  ;;

  let public_options = options |> CCList.map option_to_public
  let default_answer = CCList.hd options
  let field = create_custom_field "Select" (fun a -> Custom_field.Select (a, options))

  let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer =
    let open Custom_field in
    let answer =
      match is_admin with
      | true -> Answer.create ?admin_value:answer entity_uuid None
      | false -> Answer.create entity_uuid answer
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.Select
      ( { Public.id = id field
        ; name = name field
        ; hint = hint field
        ; validation = Validation.pure
        ; required = required field
        ; admin_override = Data.admin_override
        ; admin_input_only = Data.admin_input_only
        ; prompt_on_registration = Data.prompt_on_registration
        ; version
        }
      , public_options
      , Some answer )
  ;;

  let save () = save_custom_field field

  let save_answer answer ?admin contacts =
    CCList.map
      (fun contact ->
         let user = admin |> CCOption.value ~default:(Pool_context.Contact contact) in
         Custom_field.AnswerUpserted
           (public (CCOption.is_some admin) answer, Contact.id contact, user)
         |> Pool_event.custom_field)
      contacts
  ;;

  let filter answers operator () =
    let open Filter in
    let value =
      answers |> CCList.map (fun opt -> Option opt.Custom_field.SelectOption.id)
    in
    let query =
      Pred
        (Predicate.create
           Key.(CustomField (field |> Custom_field.id))
           operator
           (Lst value))
    in
    create None query
  ;;

  let init current_user =
    save () :: save_options field options
    |> Pool_event.handle_events Test_utils.Data.database_label current_user
  ;;
end

let admin_override_nr_field =
  Custom_field.(
    Number
      { id = Id.create ()
      ; model = Model.Contact
      ; name = Name.create [ lang ] [ lang, "admin_override_nr_field" ] |> get_exn
      ; hint = [] |> Hint.create |> get_exn
      ; validation = Validation.pure
      ; required = false |> Required.create
      ; disabled = false |> Disabled.create
      ; custom_field_group_id = None
      ; admin_hint = Data.admin_hint
      ; admin_override = true |> AdminOverride.create
      ; admin_view_only = Data.admin_view_only
      ; admin_input_only = Data.admin_input_only
      ; published_at = Data.published
      ; prompt_on_registration = false |> PromptOnRegistration.create
      ; show_on_session_close_page = false
      ; show_on_session_detail_page = false
      ; duplicate_weighting = None
      })
;;

let admin_override_nr_field_public
      ?(entity_uuid = Pool_common.Id.create ())
      is_admin
      answer_value
  =
  let open Custom_field in
  let answer =
    match is_admin with
    | true -> Answer.create ~admin_value:answer_value entity_uuid None
    | false -> Answer.create entity_uuid (Some answer_value)
  in
  let version = 0 |> Pool_common.Version.of_int in
  Public.Number
    ( { Public.id = id admin_override_nr_field
      ; name = name admin_override_nr_field
      ; hint = hint admin_override_nr_field
      ; validation = Validation.pure
      ; required = required admin_override_nr_field
      ; admin_override = admin_override admin_override_nr_field
      ; admin_input_only = admin_input_only admin_override_nr_field
      ; prompt_on_registration = prompt_on_registration admin_override_nr_field
      ; version
      }
    , Some answer )
;;

let create_admin_override_nr_field () =
  Custom_field.Created admin_override_nr_field |> Pool_event.custom_field
;;

let answer_admin_override_nr_field ?admin ~answer_value contacts =
  CCList.map
    (fun contact ->
       let user = admin |> CCOption.value ~default:(Pool_context.Contact contact) in
       Custom_field.AnswerUpserted
         ( admin_override_nr_field_public (CCOption.is_some admin) answer_value
         , Contact.id contact
         , user )
       |> Pool_event.custom_field)
    contacts
;;

let multi_select_option_data =
  let open Custom_field in
  let open CCList in
  range 0 5
  |> map (fun i -> [ lang, CCInt.to_string i ])
  |> map (Name.create [ lang ])
  |> CCList.all_ok
  |> get_exn
  |> map (fun name -> Custom_field.SelectOption.Id.create (), name)
;;

let multi_select_options =
  multi_select_option_data
  |> CCList.map (fun (id, name) -> Custom_field.SelectOption.create ~id name)
;;

let multi_select_options_public =
  multi_select_option_data
  |> CCList.map (fun (id, name) -> Custom_field.SelectOption.Public.create ~id name)
;;

let multi_select_options_by_index = CCList.map (CCList.nth multi_select_options)

let multi_select_options_public_by_index =
  CCList.map (CCList.nth multi_select_options_public)
;;

let multi_select_custom_field =
  let open Custom_field in
  MultiSelect
    ( { id = Id.create ()
      ; model = Model.Contact
      ; name = Name.create [ lang ] [ lang, "Multi select" ] |> get_exn
      ; hint = [] |> Hint.create |> get_exn
      ; validation = Validation.pure
      ; required = false |> Required.create
      ; disabled = false |> Disabled.create
      ; custom_field_group_id = None
      ; admin_hint = Data.admin_hint
      ; admin_override = Data.admin_override
      ; admin_view_only = Data.admin_view_only
      ; admin_input_only = Data.admin_input_only
      ; published_at = Data.published
      ; prompt_on_registration = false |> PromptOnRegistration.create
      ; show_on_session_close_page = false
      ; show_on_session_detail_page = false
      ; duplicate_weighting = None
      }
    , multi_select_options )
;;

let multi_select_custom_field_public
      ?(entity_uuid = Pool_common.Id.create ())
      answer_index
  =
  let open Custom_field in
  let answer =
    multi_select_options_public_by_index answer_index
    |> CCOption.pure
    |> Answer.create entity_uuid
    |> CCOption.pure
  in
  let version = 0 |> Pool_common.Version.of_int in
  Public.MultiSelect
    ( { Public.id = id multi_select_custom_field
      ; name = name multi_select_custom_field
      ; hint = hint multi_select_custom_field
      ; validation = Validation.pure
      ; required = required multi_select_custom_field
      ; admin_override = Data.admin_override
      ; admin_input_only = Data.admin_input_only
      ; prompt_on_registration = Data.prompt_on_registration
      ; version
      }
    , multi_select_options_public
    , answer )
;;

let create_multi_select () =
  let open Custom_field in
  CCList.cons
    (Created multi_select_custom_field)
    (multi_select_options
     |> CCList.map (fun o -> OptionCreated (id multi_select_custom_field, o)))
  |> CCList.map Pool_event.custom_field
;;

let answer_multi_select contacts answer_index =
  CCList.map
    (fun contact ->
       Custom_field.AnswerUpserted
         ( multi_select_custom_field_public answer_index
         , Contact.id contact
         , Pool_context.Contact contact )
       |> Pool_event.custom_field)
    contacts
;;

let publish_fields () =
  [ multi_select_custom_field; NrOfSiblings.field ]
  |> CCList.map (fun field -> Custom_field.Published field |> Pool_event.custom_field)
;;
