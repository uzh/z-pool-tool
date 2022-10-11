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

  type repo = Pool_common.Id.t * t

  let t =
    let encode ((field_id, m) : repo) = Ok (field_id, (m.id, m.name)) in
    let decode (field_id, (id, name)) = Ok (field_id, { id; name }) in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 Pool_common.Repo.Id.t (tup2 Pool_common.Repo.Id.t Name.t)))
  ;;

  let to_entity = snd
  let of_entity field_id m = field_id, m

  module Write = struct
    let t =
      let encode m = Ok (m.SelectOption.id, m.name) in
      let decode (id, name) = Ok { id; name } in
      Caqti_type.(custom ~encode ~decode (tup2 Pool_common.Repo.Id.t Name.t))
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
                , (m.validation, (m.required, (m.disabled, m.admin))) ) ) ) ) )
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
                          (tup2 Required.t (tup2 Disabled.t Admin.t)))))))))
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
    ; admin_overwrite : Admin.Overwrite.t
    ; admin_input_only : Admin.InputOnly.t
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
    }
    =
    let validation_schema schema =
      Validation.(validation |> raw_of_yojson |> schema)
    in
    match field_type with
    | FieldType.Boolean ->
      let answer =
        answer
        |> CCOption.map (fun Repo_entity_answer.{ id; value; version } ->
             value |> Utils.Bool.of_string |> Entity_answer.create ~id ~version)
      in
      Public.Boolean
        { Public.id
        ; name
        ; hint
        ; validation = Validation.pure
        ; required
        ; admin_overwrite
        ; admin_input_only
        ; answer
        }
    | FieldType.Number ->
      let answer =
        CCOption.bind answer (fun Repo_entity_answer.{ id; value; version } ->
          value
          |> CCInt.of_string
          |> CCOption.map (Entity_answer.create ~id ~version))
      in
      let validation = validation_schema Validation.Number.schema in
      Public.Number
        { Public.id
        ; name
        ; hint
        ; validation
        ; required
        ; admin_overwrite
        ; admin_input_only
        ; answer
        }
    | FieldType.Select ->
      let answer =
        answer
        |> CCOption.map (fun Repo_entity_answer.{ id; value; version } ->
             value
             |> Entity.SelectOption.Id.of_string
             |> fun selected ->
             CCList.find
               (fun (_, o) ->
                 Entity.SelectOption.Id.equal o.SelectOption.id selected)
               select_options
             |> fun (_, value) -> Entity_answer.create ~id ~version value)
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
          ; answer
          }
        , options )
    | FieldType.Text ->
      let answer =
        answer
        |> CCOption.map (fun Repo_entity_answer.{ id; value; version } ->
             value |> Entity_answer.create ~id ~version)
      in
      let validation = validation_schema Validation.Text.schema in
      Public.Text
        { Public.id
        ; name
        ; hint
        ; validation
        ; required
        ; admin_overwrite
        ; admin_input_only
        ; answer
        }
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
              , (required, (admin_overwrite, (admin_input_only, answer))) ) ) )
        ) )
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
        ; answer
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
                             Admin.Overwrite.t
                             (tup2 Admin.InputOnly.t (option Answer.t))))))))))
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
  ; admin : Admin.t
  }

let t =
  let encode _ =
    failwith
      Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
  in
  let decode
    ( id
    , ( model
      , (name, (hint, (field_type, (validation, (required, (disabled, admin))))))
      ) )
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
      ; admin
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
                        (tup2 Required.t (tup2 Disabled.t Admin.t)))))))))
;;

let to_entity
  select_options
  { id; model; name; hint; validation; field_type; required; disabled; admin }
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
      ; admin
      }
  | FieldType.Number ->
    let validation = validation_schema Validation.Number.schema in
    Number { id; model; name; hint; validation; required; disabled; admin }
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
        ; admin
        }
      , options )
  | FieldType.Text ->
    let validation = validation_schema Validation.Text.schema in
    Text { id; model; name; hint; validation; required; disabled; admin }
;;
