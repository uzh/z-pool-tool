let get_or_failwith = Pool_common.Utils.get_or_failwith

let create pool =
  let open CCFun in
  let open Custom_field in
  let system_languages = Pool_common.Language.all in
  let admin = Admin.{ hint = None; overwrite = false |> Overwrite.create } in
  let data =
    [ "Hair color", Model.Contact, Some "Hint", [], true, false, FieldType.Text
    ; "Age", Model.Contact, Some "Hint", [], true, false, FieldType.Number
    ; ( "Experiment Attribute"
      , Model.Experiment
      , Some "Hint"
      , []
      , true
      , false
      , FieldType.Text )
    ]
  in
  let events =
    CCList.map
      (fun (name, model, hint, validation, required, disabled, field_type) ->
        let field =
          let name =
            system_languages
            |> CCList.map (fun lang -> lang, name)
            |> Name.create system_languages
            |> get_or_failwith
          in
          let hint =
            system_languages
            |> CCList.filter_map (fun lang ->
                 hint |> CCOption.map (fun h -> lang, h))
            |> Hint.create
            |> get_or_failwith
          in
          let required = required |> Required.create in
          let disabled = disabled |> Disabled.create in
          create field_type model name hint validation required disabled admin
          |> get_or_failwith
        in
        Custom_field.Created field)
      data
  in
  let%lwt () = Lwt_list.iter_s (Custom_field.handle_event pool) events in
  Lwt.return_unit
;;
