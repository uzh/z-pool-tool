open Entity
module Common = Pool_common
module Answer = Repo_entity_answer

let encode_yojson of_t t = t |> of_t |> Yojson.Safe.to_string |> CCResult.return

let decode_yojson t_of_yojson field t =
  let read s = s |> Yojson.Safe.from_string |> t_of_yojson in
  try Ok (read t) with
  | _ ->
    Error
      Pool_common.(Utils.error_to_string Language.En Message.(Invalid field))
;;

module Model = struct
  include Model

  let t =
    let encode = Utils.fcn_ok value in
    let decode m =
      m |> create |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Name = struct
  include Name

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_common.Message.Field.Name in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Hint = struct
  include Hint

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_common.Message.Field.Hint in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module FieldType = struct
  include FieldType

  let t =
    let encode = Utils.fcn_ok value in
    let decode m =
      m |> create |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Validation = struct
  include Validation

  let t =
    let encode = encode_yojson CCFun.id in
    let decode = decode_yojson CCFun.id Pool_common.Message.Field.Validation in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Required = struct
  include Required

  let t = Caqti_type.bool
end

module Disabled = struct
  include Disabled

  let t = Caqti_type.bool
end

module PublishedAt = struct
  include PublishedAt

  let t = Caqti_type.ptime
end

module Admin = struct
  include Admin

  module Hint = struct
    include Hint

    let t = Caqti_type.string
  end

  module Overwrite = struct
    include Overwrite

    let t = Caqti_type.bool
  end

  module ViewOnly = struct
    include ViewOnly

    let t = Caqti_type.bool
  end

  module InputOnly = struct
    include InputOnly

    let t = Caqti_type.bool
  end

  let t =
    let encode m =
      Ok (m.Admin.hint, (m.overwrite, (m.view_only, m.input_only)))
    in
    let decode (hint, (overwrite, (view_only, input_only))) =
      Ok { hint; overwrite; view_only; input_only }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 (option Hint.t) (tup2 Overwrite.t (tup2 ViewOnly.t InputOnly.t))))
  ;;
end

module Option = struct
  open Entity.SelectOption

  module Id = struct
    include Id

    let t =
      let encode = Utils.fcn_ok value in
      let decode = Utils.fcn_ok of_string in
      Caqti_type.(custom ~encode ~decode string)
    ;;
  end

  type repo = Pool_common.Id.t * t

  let t =
    let encode ((field_id, m) : repo) =
      Ok (field_id, (m.id, (m.name, m.published_at)))
    in
    let decode (field_id, (id, (name, published_at))) =
      Ok (field_id, { id; name; published_at })
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_common.Repo.Id.t
           (tup2 Id.t (tup2 Name.t (option PublishedAt.t)))))
  ;;

  module Public = struct
    open Public

    type repo = Pool_common.Id.t * t

    let t =
      let encode ((field_id, m) : repo) = Ok (field_id, (m.id, m.name)) in
      let decode (field_id, (id, name)) = Ok (field_id, { id; name }) in
      Caqti_type.(
        custom ~encode ~decode (tup2 Pool_common.Repo.Id.t (tup2 Id.t Name.t)))
    ;;

    let to_entity = snd
    let of_entity field_id m = field_id, m
  end

  let to_entity = snd
  let of_entity field_id m = field_id, m

  (* TODO: Merge those modules? *)
  module Insert = struct
    let t =
      let encode ((field_id, m) : repo) = Ok (field_id, (m.id, m.name)) in
      let decode _ =
        failwith
          Pool_common.(
            Message.WriteOnlyModel |> Utils.error_to_string Language.En)
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (tup2 Pool_common.Repo.Id.t (tup2 Pool_common.Repo.Id.t Name.t)))
    ;;
  end

  module Update = struct
    let t =
      let encode m = Ok (m.SelectOption.id, (m.name, m.published_at)) in
      let decode _ =
        failwith
          Pool_common.(
            Message.WriteOnlyModel |> Utils.error_to_string Language.En)
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (tup2 Pool_common.Repo.Id.t (tup2 Name.t (option PublishedAt.t))))
    ;;
  end
end

module Write = struct
  let of_entity (t : t) =
    Write.
      { id = id t
      ; model = model t
      ; name = name t
      ; hint = hint t
      ; validation = Entity.validation_to_yojson t
      ; field_type = field_type t
      ; required = required t
      ; disabled = disabled t
      ; custom_field_group_id = group_id t
      ; admin = admin t
      }
  ;;

  let t =
    let open Write in
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.model
          , ( m.name
            , ( m.hint
              , ( m.field_type
                , ( m.validation
                  , ( m.required
                    , (m.disabled, (m.custom_field_group_id, m.admin)) ) ) ) )
            ) ) )
    in
    let decode _ =
      failwith
        Pool_common.(
          Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Common.Repo.Id.t
           (tup2
              Model.t
              (tup2
                 Name.t
                 (tup2
                    Hint.t
                    (tup2
                       FieldType.t
                       (tup2
                          Validation.t
                          (tup2
                             Required.t
                             (tup2
                                Disabled.t
                                (tup2 (option Common.Repo.Id.t) Admin.t))))))))))
  ;;
end

module Public = struct
  type repo =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; validation : Yojson.Safe.t
    ; field_type : FieldType.t
    ; required : Required.t
    ; custom_field_group_id : Group.Id.t option
    ; admin_overwrite : Admin.Overwrite.t
    ; admin_input_only : Admin.InputOnly.t
    ; version : Pool_common.Version.t option
    ; answer : Repo_entity_answer.repo option
    }

  let to_entity
    select_options
    { id
    ; name
    ; hint
    ; validation
    ; field_type
    ; required
    ; admin_overwrite
    ; admin_input_only
    ; answer
    ; version
    ; _
    }
    answers
    =
    let open CCOption.Infix in
    let validation_schema schema =
      Validation.(validation |> raw_of_yojson |> schema)
    in
    let version =
      CCOption.value ~default:(Pool_common.Version.create ()) version
    in
    match field_type with
    | FieldType.Boolean ->
      let answer =
        answer
        >|= fun { Answer.id; value } ->
        value |> Utils.Bool.of_string |> Entity_answer.create ~id
      in
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
    | FieldType.Number ->
      let answer =
        answer
        >>= fun Answer.{ id; value } ->
        value |> CCInt.of_string >|= Entity_answer.create ~id
      in
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
        let open SelectOption in
        answer
        >|= fun Answer.{ id; value } ->
        value
        |> Id.of_string
        |> fun selected ->
        CCList.find
          (fun (_, { SelectOption.Public.id; _ }) -> Id.equal id selected)
          select_options
        |> snd
        |> Entity_answer.create ~id
      in
      let options =
        CCList.filter_map
          (fun (field_id, option) ->
            if Pool_common.Id.equal field_id id then Some option else None)
          select_options
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
        , options
        , answer )
    | FieldType.MultiSelect ->
      let options =
        CCList.filter_map
          (fun (field_id, option) ->
            if Pool_common.Id.equal field_id id then Some option else None)
          select_options
      in
      let answers =
        let open SelectOption in
        answers
        |> CCList.filter_map (fun Answer.{ id; value } ->
             value
             |> Id.of_string
             |> fun selected ->
             CCList.find_opt
               (fun { SelectOption.Public.id; _ } -> Id.equal id selected)
               options
             >|= Entity_answer.create ~id)
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
        , options
        , answers )
    | FieldType.Text ->
      let answer =
        answer >|= fun Answer.{ id; value } -> value |> Entity_answer.create ~id
      in
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

  let group_fields lst =
    let open CCList in
    fold_left
      (fun acc (field : repo) ->
        match assoc_opt ~eq:Id.equal field.id acc with
        | None -> acc @ [ field.id, [ field ] ]
        | Some fields ->
          Assoc.set ~eq:Id.equal field.id (fields @ [ field ]) acc)
      []
      lst
    |> map snd
  ;;

  let single_to_entity options field_list =
    let fst = CCList.hd field_list in
    field_list
    |> CCList.filter_map (fun field -> field.answer)
    |> to_entity options fst
  ;;

  let to_ungrouped_entities select_options fields =
    let to_entity = single_to_entity select_options in
    fields |> group_fields |> CCList.map to_entity
  ;;

  let to_grouped_entities select_options groups fields =
    let to_entity = single_to_entity select_options in
    let partition_map fields { Group.id; _ } =
      CCList.fold_left
        (fun (of_group, rest) (fields : repo list) ->
          let ({ custom_field_group_id; _ } : repo) = CCList.hd fields in
          CCOption.map_or
            ~default:false
            (fun group_id -> Pool_common.Id.equal group_id id)
            custom_field_group_id
          |> function
          | true -> of_group @ [ fields |> to_entity ], rest
          | false -> of_group, rest @ [ fields ])
        ([], [])
        fields
    in
    let grouped, ungrouped =
      CCList.fold_left
        (fun (groups, fields) group ->
          let of_group, rest = partition_map fields group in
          let group =
            Group.{ Public.id = group.id; name = group.name; fields = of_group }
          in
          CCList.append groups [ group ], rest)
        ([], group_fields fields)
        groups
    in
    ( grouped
      |> CCList.filter (fun g -> CCList.is_empty g.Group.Public.fields |> not)
    , ungrouped |> CCList.map to_entity )
  ;;

  let t =
    let encode _ =
      failwith
        Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
    in
    let decode
      ( id
      , ( name
        , ( hint
          , ( validation
            , ( field_type
              , ( required
                , ( custom_field_group_id
                  , (admin_overwrite, (admin_input_only, (answer, version))) )
                ) ) ) ) ) )
      =
      Ok
        { id
        ; name
        ; hint
        ; validation
        ; field_type
        ; required
        ; admin_overwrite
        ; admin_input_only
        ; custom_field_group_id
        ; answer
        ; version
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Common.Repo.Id.t
           (tup2
              Name.t
              (tup2
                 Hint.t
                 (tup2
                    Validation.t
                    (tup2
                       FieldType.t
                       (tup2
                          Required.t
                          (tup2
                             (option Common.Repo.Id.t)
                             (tup2
                                Admin.Overwrite.t
                                (tup2
                                   Admin.InputOnly.t
                                   (tup2
                                      (option Answer.t)
                                      (option Common.Repo.Version.t))))))))))))
  ;;
end

type repo =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; field_type : FieldType.t
  ; validation : Yojson.Safe.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; custom_field_group_id : Group.Id.t option
  ; admin : Admin.t
  ; published_at : PublishedAt.t option
  }

let t =
  let encode _ =
    failwith
      Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
  in
  let decode
    ( id
    , ( model
      , ( name
        , ( hint
          , ( field_type
            , ( validation
              , ( required
                , (disabled, (custom_field_group_id, (admin, published_at))) )
              ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { id
      ; model
      ; name
      ; hint
      ; field_type
      ; validation
      ; required
      ; disabled
      ; custom_field_group_id
      ; admin
      ; published_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common.Repo.Id.t
         (tup2
            Model.t
            (tup2
               Name.t
               (tup2
                  Hint.t
                  (tup2
                     FieldType.t
                     (tup2
                        Validation.t
                        (tup2
                           Required.t
                           (tup2
                              Disabled.t
                              (tup2
                                 (option Common.Repo.Id.t)
                                 (tup2 Admin.t (option PublishedAt.t))))))))))))
;;

let to_entity
  select_options
  { id
  ; model
  ; name
  ; hint
  ; validation
  ; field_type
  ; required
  ; disabled
  ; custom_field_group_id
  ; admin
  ; published_at
  }
  =
  let validation_schema schema =
    Validation.(validation |> raw_of_yojson |> schema)
  in
  match field_type with
  | FieldType.Boolean ->
    Boolean
      { id
      ; model
      ; name
      ; hint
      ; validation = Validation.pure
      ; required
      ; disabled
      ; custom_field_group_id
      ; admin
      ; published_at
      }
  | FieldType.Number ->
    let validation = validation_schema Validation.Number.schema in
    Number
      { id
      ; model
      ; name
      ; hint
      ; validation
      ; required
      ; disabled
      ; custom_field_group_id
      ; admin
      ; published_at
      }
  | FieldType.Select ->
    let options =
      CCList.filter_map
        (fun (field_id, option) ->
          if Pool_common.Id.equal field_id id then Some option else None)
        select_options
    in
    Select
      ( { id
        ; model
        ; name
        ; hint
        ; validation = Validation.pure
        ; required
        ; disabled
        ; custom_field_group_id
        ; admin
        ; published_at
        }
      , options )
  | FieldType.MultiSelect ->
    let options =
      CCList.filter_map
        (fun (field_id, option) ->
          if Pool_common.Id.equal field_id id then Some option else None)
        select_options
    in
    MultiSelect
      ( { id
        ; model
        ; name
        ; hint
        ; validation = Validation.pure
        ; required
        ; disabled
        ; custom_field_group_id
        ; admin
        ; published_at
        }
      , options )
  | FieldType.Text ->
    let validation = validation_schema Validation.Text.schema in
    Text
      { id
      ; model
      ; name
      ; hint
      ; validation
      ; required
      ; disabled
      ; custom_field_group_id
      ; admin
      ; published_at
      }
;;
