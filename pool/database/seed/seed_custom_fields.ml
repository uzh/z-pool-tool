let get_or_failwith = Pool_common.Utils.get_or_failwith

let create pool =
  let open CCFun in
  let open Custom_field in
  let system_languages = Pool_common.Language.all in
  let make_names values =
    values |> Name.create system_languages |> get_or_failwith
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
  let education_group =
    (Model.Contact, Pool_common.Language.[ En, "Education"; De, "Bildung" ])
    |> create_group
  in
  let language_group =
    (Model.Contact, Pool_common.Language.[ En, "Languages"; De, "Sprachen" ])
    |> create_group
  in
  let groups = [ education_group; language_group ] in
  let education_options =
    [ "Bachelor"; "Master"; "Phd" ]
    |> CCList.map (fun value ->
         CCList.map (fun lang -> lang, value) system_languages)
  in
  let languge_level_options =
    Pool_common.Language.
      [ [ En, "No knowledge"; De, "Keine Kenntnisse" ]
      ; [ En, "Beginner"; De, "Schlecht" ]
      ; [ En, "Intermediate"; De, "Mittel" ]
      ; [ En, "Fluent"; De, "Fliessend" ]
      ; [ En, "Native speaker"; De, "Muttersprache" ]
      ]
  in
  let research_interest_options =
    Pool_common.Language.
      [ [ En, "Environmental Economics"; De, "Umweltökonomie" ]
      ; [ En, "Development Economics"; De, "Entwicklungsökonomie" ]
      ; [ En, "Technological Change & Innovation"
        ; De, "Technologischer Wandel & Innovation"
        ]
      ; [ En, "Neuroeconomics"; De, "Neuroökonomie" ]
      ; [ En, "Behavioral Economics"; De, "Verhaltensökonomie" ]
      ; [ En, "International Trade"; De, "Internationaler Handel" ]
      ; [ En, "Macroeconomics"; De, "Makroökonomie" ]
      ; [ En, "Public Economics"; De, "Öffentlicher Sektor" ]
      ; [ En, "Economic Theory"; De, "Wirtschaftstheorie" ]
      ; [ En, "Organizational Economics"; De, "Organisationsökonomie" ]
      ; [ En, "Political Economy"; De, "Politische Ökonomie" ]
      ; [ En, "Econometrics"; De, "Ökonometrie" ]
      ; [ En, "Economic History"; De, "Wirtschaftsgeschichte" ]
      ; [ En, "Financial Markets"; De, "Finanzmärkte" ]
      ; [ En, "Industrial Organization"; De, "Industrielle Organisation" ]
      ; [ En, "Labor Economics"; De, "Arbeitsökonomie" ]
      ; [ En, "Education & Health"; De, "Gesundheits- und Bildungsökonomie" ]
      ; [ En, "Economics of Institutions"; De, "Institutionenökonomik" ]
      ]
  in
  let profession_options =
    Pool_common.Language.
      [ [ En, "Bank clerk"; De, "Bankangestellter" ]
      ; [ En, "Consultant"; De, "Berater" ]
      ; [ En, "Accountant"; De, "Buchhalter" ]
      ; [ En, "Management assistant"; De, "Büroangestellte" ]
      ; [ En, "Research"; De, "Forschung" ]
      ; [ En, "Nurse"; De, "Krankenpfleger-/schwester" ]
      ; [ En, "Apprenticeship"; De, "Lehre" ]
      ; [ En, "metalworker"; De, "Metallbauer" ]
      ; [ En, "Self-employeed"; De, "Selbständiger" ]
      ; [ En, "No profession"; De, "Kein Beruf" ]
      ; [ En, "Other profession"; De, "Anderer Beruf" ]
      ]
  in
  let data =
    [ ( Pool_common.Language.
          [ En, "Research interests"; De, "Forschungsinteressen" ]
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.MultiSelect
      , research_interest_options
      , None )
    ; ( Pool_common.Language.[ En, "Level of english"; De, "Englischkenntnisse" ]
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , languge_level_options
      , Some language_group )
    ; ( Pool_common.Language.[ En, "Level of german"; De, "Deutschkenntnisse" ]
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , languge_level_options
      , Some language_group )
    ; ( Pool_common.Language.[ En, "Highest degree"; De, "Höchster Abschluss" ]
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , education_options
      , Some education_group )
    ; ( Pool_common.Language.[ En, "Profession"; De, "Beruf" ]
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , profession_options
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
        let field =
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
        let create_field_event = Custom_field.Created field in
        let option_events =
          CCList.map
            (fun option ->
              let name = make_names option in
              let option = SelectOption.create name in
              OptionCreated (field_id, option))
            options
        in
        let publis_field_event = Custom_field.Published field in
        [ create_field_event ] @ option_events @ [ publis_field_event ])
      data
  in
  let%lwt () =
    (field_events |> CCList.flatten) @ group_events
    |> Lwt_list.iter_s (Custom_field.handle_event pool)
  in
  Lwt.return_unit
;;
