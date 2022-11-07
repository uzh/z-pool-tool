let get_or_failwith = Pool_common.Utils.get_or_failwith

let create pool =
  let open CCFun in
  let open Custom_field in
  let system_languages = Pool_common.Language.all in
  let make_names value =
    system_languages
    |> CCList.map (fun lang -> lang, value)
    |> Name.create system_languages
    |> get_or_failwith
  in
  let admin =
    Admin.
      { hint = None
      ; overwrite = true |> Overwrite.create
      ; view_only = false |> ViewOnly.create
      ; input_only = false |> InputOnly.create
      }
  in
  let create_group (model, name) =
    let name = make_names name in
    Custom_field.Group.create model name
  in
  let education_group = (Model.Contact, "Education") |> create_group in
  let language_group = (Model.Contact, "Languages") |> create_group in
  let groups = [ education_group; language_group ] in
  let education_options = [ "Bachelor"; "Master"; "Phd" ] in
  let languge_level_options = [ "Beginner"; "Intermediate"; "Advanced" ] in
  let research_interest_options =
    [ "Environmental Economics"
    ; "Development Economics"
    ; "Technological Change & Innovation"
    ; "Neuroeconomics"
    ; "Behavioral Economics"
    ; "International Trade"
    ; "Macroeconomics"
    ; "Public Economics"
    ; "Economic Theory"
    ; "Organizational Economics"
    ; "Political Economy"
    ; "Econometrics"
    ; "Economic History"
    ; "Financial Markets"
    ; "Industrial Organization"
    ; "Labor Economics"
    ; "Education & Health"
    ; "Economics of Institutions"
    ]
  in
  let data =
    [ ( "Research interests"
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.MultiSelect
      , research_interest_options
      , None )
    ; ( "Mother tongue"
      , Model.Contact
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Text
      , []
      , Some language_group )
    ; ( "Level of english"
      , Model.Contact
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Select
      , languge_level_options
      , Some language_group )
    ; ( "Nr. of siblings"
      , Model.Contact
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Number
      , []
      , None )
    ; ( "Degree"
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , education_options
      , Some education_group )
    ; ( "Year of studies"
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Number
      , []
      , Some education_group )
    ; ( "Experiment Attribute"
      , Model.Experiment
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Text
      , []
      , None )
    ]
  in
  let group_events =
    CCList.map (fun group -> Custom_field.GroupCreated group) groups
  in
  let field_events =
    CCList.map
      (fun ( name
           , model
           , hint
           , validation
           , required
           , disabled
           , field_type
           , options
           , group ) ->
        let field_id = Custom_field.Id.create () in
        let field_event =
          let name = make_names name in
          let hint =
            system_languages
            |> CCList.filter_map (fun lang ->
                 hint |> CCOption.map (fun h -> lang, h))
            |> Hint.create
            |> get_or_failwith
          in
          let required = required |> Required.create in
          let disabled = disabled |> Disabled.create in
          let field =
            create
              ~id:field_id
              field_type
              model
              name
              hint
              validation
              required
              disabled
              (group |> CCOption.map (fun g -> g.Group.id))
              admin
            |> get_or_failwith
          in
          Custom_field.Created field
        in
        let option_events =
          CCList.map
            (fun option ->
              let name = make_names option in
              let option = SelectOption.create name in
              OptionCreated (field_id, option))
            options
        in
        field_event :: option_events)
      data
  in
  let%lwt () =
    (field_events |> CCList.flatten) @ group_events
    |> Lwt_list.iter_s (Custom_field.handle_event pool)
  in
  Lwt.return_unit
;;
