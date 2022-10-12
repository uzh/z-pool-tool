let get_or_failwith = Pool_common.Utils.get_or_failwith

let create pool =
  let open CCFun in
  let open Custom_field in
  let system_languages = Pool_common.Language.all in
  let admin =
    Admin.
      { hint = None
      ; overwrite = false |> Overwrite.create
      ; view_only = false |> ViewOnly.create
      ; input_only = false |> InputOnly.create
      }
  in
  let education_options = [ "Bachelor"; "Master"; "Phd" ] in
  let data =
    [ ( "Hair color"
      , Model.Contact
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Text
      , [] )
    ; "Age", Model.Contact, Some "Hint", [], true, false, FieldType.Number, []
    ; ( "Education"
      , Model.Contact
      , None
      , []
      , true
      , false
      , FieldType.Select
      , education_options )
    ; ( "Experiment Attribute"
      , Model.Experiment
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Text
      , [] )
    ]
  in
  let events =
    CCList.map
      (fun ( name
           , model
           , hint
           , validation
           , required
           , disabled
           , field_type
           , options ) ->
        let field_id = Custom_field.Id.create () in
        let make_names value =
          system_languages
          |> CCList.map (fun lang -> lang, value)
          |> Name.create system_languages
          |> get_or_failwith
        in
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
    events |> CCList.flatten |> Lwt_list.iter_s (Custom_field.handle_event pool)
  in
  Lwt.return_unit
;;
