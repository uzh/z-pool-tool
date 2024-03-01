open CCFun
open Entity
open Ppx_yojson_conv_lib.Yojson_conv
module Answer = Repo_entity_answer

let encode_yojson of_t t = t |> of_t |> Yojson.Safe.to_string |> CCResult.return

let decode_yojson t_of_yojson field t =
  let read s = s |> Yojson.Safe.from_string |> t_of_yojson in
  try Ok (read t) with
  | _ ->
    Error
      (Pool_message.(Error.Invalid field)
       |> Pool_common.Utils.error_to_string Language.En)
;;

type multi_select_answer = SelectOption.Id.t list [@@deriving yojson]

module Model = Pool_common.Repo.Model.SelectorType (Model)

module Name = struct
  include Name

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_message.Field.Name in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Hint = struct
  include Hint

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_message.Field.Hint in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module FieldType = struct
  include Pool_common.Repo.Model.SelectorType (FieldType)
  include FieldType
end

module Validation = struct
  include Validation

  let t =
    let encode = encode_yojson CCFun.id in
    let decode = decode_yojson CCFun.id Pool_message.Field.Validation in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Required = struct
  include Required

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module Disabled = struct
  include Disabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module PublishedAt = struct
  include PublishedAt

  let t = Pool_common.Repo.make_caqti_type Caqti_type.ptime create value
end

module AdminHint = struct
  include AdminHint

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module AdminOverride = struct
  include AdminOverride

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module AdminViewOnly = struct
  include AdminViewOnly

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module AdminInputOnly = struct
  include AdminInputOnly

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module PromptOnRegistration = struct
  include PromptOnRegistration

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module Option = struct
  open Entity.SelectOption

  module Id = struct
    include Id

    let t =
      Pool_common.Repo.make_caqti_type
        Caqti_type.string
        (of_string %> CCResult.return)
        value
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
        (t2 Pool_common.Repo.Id.t (t2 Id.t (t2 Name.t (option PublishedAt.t)))))
  ;;

  module Public = struct
    open Public

    type repo = Pool_common.Id.t * t

    let t =
      let encode ((field_id, m) : repo) = Ok (field_id, (m.id, m.name)) in
      let decode (field_id, (id, name)) = Ok (field_id, { id; name }) in
      Caqti_type.(
        custom ~encode ~decode (t2 Pool_common.Repo.Id.t (t2 Id.t Name.t)))
    ;;

    let to_entity = snd
    let of_entity field_id m = field_id, m
  end

  let to_entity = snd
  let of_entity field_id m = field_id, m

  module Insert = struct
    let t =
      let encode ((field_id, m) : repo) = Ok (field_id, (m.id, m.name)) in
      let decode _ =
        Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (t2 Pool_common.Repo.Id.t (t2 Pool_common.Repo.Id.t Name.t)))
    ;;
  end

  module Update = struct
    let t =
      let encode m = Ok (m.SelectOption.id, m.name) in
      let decode _ =
        Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
      in
      Caqti_type.(custom ~encode ~decode (t2 Pool_common.Repo.Id.t Name.t))
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
      ; admin_hint = admin_hint t
      ; admin_override = admin_override t
      ; admin_view_only = admin_view_only t
      ; admin_input_only = admin_input_only t
      ; prompt_on_registration = prompt_on_registration t
      ; show_on_session_close_page = show_on_session_close_page t
      ; show_on_session_detail_page = show_on_session_detail_page t
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
                    , ( m.disabled
                      , ( m.custom_field_group_id
                        , ( m.admin_hint
                          , ( m.admin_override
                            , ( m.admin_view_only
                              , ( m.admin_input_only
                                , ( m.prompt_on_registration
                                  , ( m.show_on_session_close_page
                                    , m.show_on_session_detail_page ) ) ) ) ) )
                        ) ) ) ) ) ) ) ) )
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              Model.t
              (t2
                 Name.t
                 (t2
                    Hint.t
                    (t2
                       FieldType.t
                       (t2
                          Validation.t
                          (t2
                             Required.t
                             (t2
                                Disabled.t
                                (t2
                                   (option Pool_common.Repo.Id.t)
                                   (t2
                                      (option AdminHint.t)
                                      (t2
                                         AdminOverride.t
                                         (t2
                                            AdminViewOnly.t
                                            (t2
                                               AdminInputOnly.t
                                               (t2
                                                  PromptOnRegistration.t
                                                  (t2 bool bool))))))))))))))))
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
    ; admin_override : AdminOverride.t
    ; admin_input_only : AdminInputOnly.t
    ; prompt_on_registration : PromptOnRegistration.t
    ; answer_id : Pool_common.Id.t option
    ; answer_entity_uuid : Pool_common.Id.t option
    ; answer_value : string option
    ; answer_admin_value : string option
    ; version : Pool_common.Version.t option
    ; admin_version : Pool_common.Version.t option
    }
  [@@deriving show, eq]

  let create_answer
    id
    entity_uuid
    ~is_admin
    ~admin_override
    ~answer_value
    ~answer_admin_value
    parse_value
    =
    let open CCOption.Infix in
    match id, entity_uuid with
    | None, None | None, Some _ | Some _, None -> None
    | Some id, Some entity_uuid ->
      let value = answer_value >>= parse_value in
      let admin_value =
        if is_admin && admin_override
        then answer_admin_value >>= parse_value
        else None
      in
      Entity_answer.create ~id ?admin_value entity_uuid value |> CCOption.pure
  ;;

  let to_entity
    is_admin
    select_options
    { id
    ; name
    ; hint
    ; validation
    ; field_type
    ; required
    ; admin_override
    ; admin_input_only
    ; prompt_on_registration
    ; answer_id
    ; answer_entity_uuid
    ; answer_value
    ; answer_admin_value
    ; version
    ; admin_version
    ; _
    }
    =
    let open CCOption.Infix in
    let validation_schema schema =
      Validation.(validation |> raw_of_yojson |> schema)
    in
    let version =
      (if is_admin then admin_version else version)
      |> CCOption.value ~default:(Pool_common.Version.create ())
    in
    match field_type with
    | FieldType.Boolean ->
      let answer =
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          CCFun.(Utils.Bool.of_string %> CCOption.pure)
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
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          CCFun.(Entity.Ptime.date_of_string %> CCResult.to_opt)
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
    | FieldType.Number ->
      let answer =
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          CCInt.of_string
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
        let create value =
          value
          |> Id.of_string
          |> fun selected ->
          CCList.find_opt
            (fun (_, { SelectOption.Public.id; _ }) -> Id.equal id selected)
            select_options
          >|= snd
        in
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          create
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
          ; admin_override
          ; admin_input_only
          ; prompt_on_registration
          ; version
          }
        , options
        , answer )
    | FieldType.MultiSelect ->
      let select_options =
        CCList.filter_map
          (fun (field_id, option) ->
            if Pool_common.Id.equal field_id id then Some option else None)
          select_options
      in
      let validation = validation_schema Validation.MultiSelect.schema in
      let answer =
        let open SelectOption in
        let create value =
          let options =
            try
              value |> Yojson.Safe.from_string |> multi_select_answer_of_yojson
            with
            | _ -> []
          in
          match options with
          | [] -> None
          | options ->
            options
            |> CCList.filter_map (fun option_id ->
              CCList.find_opt
                (fun { SelectOption.Public.id; _ } -> Id.equal id option_id)
                select_options)
            |> CCOption.pure
        in
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          create
      in
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
        , select_options
        , answer )
    | FieldType.Text ->
      let answer =
        create_answer
          answer_id
          answer_entity_uuid
          ~is_admin
          ~admin_override
          ~answer_value
          ~answer_admin_value
          CCOption.pure
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

  let to_grouped_entities is_admin select_options groups fields =
    let to_entity = to_entity is_admin select_options in
    let partition_map fields { Group.id; _ } =
      CCList.partition_filter_map
        (fun (field : repo) ->
          match
            field.custom_field_group_id
            |> CCOption.map_or ~default:false (Id.equal id)
          with
          | true -> `Left (to_entity field)
          | false -> `Right field)
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
        ([], fields)
        groups
    in
    ( grouped
      |> CCList.filter (fun g -> CCList.is_empty g.Group.Public.fields |> not)
    , ungrouped |> CCList.map to_entity )
  ;;

  let to_ungrouped_entities is_admin select_options fields =
    fields |> CCList.map (to_entity is_admin select_options)
  ;;

  let t =
    let encode _ =
      Pool_message.Error.ReadOnlyModel |> Pool_common.Utils.failwith
    in
    let decode
      ( id
      , ( name
        , ( hint
          , ( validation
            , ( field_type
              , ( required
                , ( custom_field_group_id
                  , ( admin_override
                    , ( admin_input_only
                      , ( prompt_on_registration
                        , ( answer_id
                          , ( answer_entity_uuid
                            , ( answer_value
                              , (answer_admin_value, (version, admin_version))
                              ) ) ) ) ) ) ) ) ) ) ) ) )
      =
      Ok
        { id
        ; name
        ; hint
        ; validation
        ; field_type
        ; required
        ; admin_override
        ; admin_input_only
        ; prompt_on_registration
        ; custom_field_group_id
        ; answer_id
        ; answer_entity_uuid
        ; answer_value
        ; answer_admin_value
        ; version
        ; admin_version
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              Name.t
              (t2
                 Hint.t
                 (t2
                    Validation.t
                    (t2
                       FieldType.t
                       (t2
                          Required.t
                          (t2
                             (option Pool_common.Repo.Id.t)
                             (t2
                                AdminOverride.t
                                (t2
                                   AdminInputOnly.t
                                   (t2
                                      PromptOnRegistration.t
                                      (t2
                                         (option Pool_common.Repo.Id.t)
                                         (t2
                                            (option Pool_common.Repo.Id.t)
                                            (t2
                                               (option Caqti_type.string)
                                               (t2
                                                  (option Caqti_type.string)
                                                  (t2
                                                     (option
                                                        Pool_common.Repo.Version
                                                        .t)
                                                     (option
                                                        Pool_common.Repo.Version
                                                        .t)))))))))))))))))
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
  ; admin_hint : AdminHint.t option
  ; admin_override : AdminOverride.t
  ; admin_view_only : AdminViewOnly.t
  ; admin_input_only : AdminInputOnly.t
  ; prompt_on_registration : PromptOnRegistration.t
  ; published_at : PublishedAt.t option
  ; show_on_session_close_page : bool
  ; show_on_session_detail_page : bool
  }

let t =
  let encode _ =
    Pool_message.Error.ReadOnlyModel |> Pool_common.Utils.failwith
  in
  let decode
    ( id
    , ( model
      , ( name
        , ( hint
          , ( field_type
            , ( validation
              , ( required
                , ( disabled
                  , ( custom_field_group_id
                    , ( admin_hint
                      , ( admin_override
                        , ( admin_view_only
                          , ( admin_input_only
                            , ( prompt_on_registration
                              , ( published_at
                                , ( show_on_session_close_page
                                  , show_on_session_detail_page ) ) ) ) ) ) ) )
                  ) ) ) ) ) ) ) )
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
      ; admin_hint
      ; admin_override
      ; admin_view_only
      ; admin_input_only
      ; prompt_on_registration
      ; published_at
      ; show_on_session_close_page
      ; show_on_session_detail_page
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Pool_common.Repo.Id.t
         (t2
            Model.t
            (t2
               Name.t
               (t2
                  Hint.t
                  (t2
                     FieldType.t
                     (t2
                        Validation.t
                        (t2
                           Required.t
                           (t2
                              Disabled.t
                              (t2
                                 (option Pool_common.Repo.Id.t)
                                 (t2
                                    (option AdminHint.t)
                                    (t2
                                       AdminOverride.t
                                       (t2
                                          AdminViewOnly.t
                                          (t2
                                             AdminInputOnly.t
                                             (t2
                                                PromptOnRegistration.t
                                                (t2
                                                   (option PublishedAt.t)
                                                   (t2 bool bool)))))))))))))))))
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
  ; admin_hint
  ; admin_override
  ; admin_view_only
  ; admin_input_only
  ; prompt_on_registration
  ; published_at
  ; show_on_session_close_page
  ; show_on_session_detail_page
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
      ; admin_hint
      ; admin_override
      ; admin_view_only
      ; admin_input_only
      ; prompt_on_registration
      ; published_at
      ; show_on_session_close_page
      ; show_on_session_detail_page
      }
  | FieldType.Date ->
    Date
      { id
      ; model
      ; name
      ; hint
      ; validation = Validation.pure
      ; required
      ; disabled
      ; custom_field_group_id
      ; admin_hint
      ; admin_override
      ; admin_view_only
      ; admin_input_only
      ; prompt_on_registration
      ; published_at
      ; show_on_session_close_page
      ; show_on_session_detail_page
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
      ; admin_hint
      ; admin_override
      ; admin_view_only
      ; admin_input_only
      ; prompt_on_registration
      ; published_at
      ; show_on_session_close_page
      ; show_on_session_detail_page
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
        ; admin_hint
        ; admin_override
        ; admin_view_only
        ; admin_input_only
        ; prompt_on_registration
        ; published_at
        ; show_on_session_close_page
        ; show_on_session_detail_page
        }
      , options )
  | FieldType.MultiSelect ->
    let options =
      CCList.filter_map
        (fun (field_id, option) ->
          if Pool_common.Id.equal field_id id then Some option else None)
        select_options
    in
    let validation = validation_schema Validation.MultiSelect.schema in
    MultiSelect
      ( { id
        ; model
        ; name
        ; hint
        ; validation
        ; required
        ; disabled
        ; custom_field_group_id
        ; admin_hint
        ; admin_override
        ; admin_view_only
        ; admin_input_only
        ; prompt_on_registration
        ; published_at
        ; show_on_session_close_page
        ; show_on_session_detail_page
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
      ; admin_hint
      ; admin_override
      ; admin_view_only
      ; admin_input_only
      ; prompt_on_registration
      ; published_at
      ; show_on_session_close_page
      ; show_on_session_detail_page
      }
;;
