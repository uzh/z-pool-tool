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

  let t =
    let encode m = Ok (m.Admin.hint, m.overwrite) in
    let decode (hint, overwrite) = Ok { hint; overwrite } in
    Caqti_type.(custom ~encode ~decode (tup2 (option Hint.t) Overwrite.t))
  ;;
end

module Write = struct
  let of_entity (t : t) =
    Write.
      { id = get_id t
      ; model = get_model t
      ; name = get_name t
      ; hint = get_hint t
      ; validation = Entity.validation_to_yojson t
      ; field_type = get_field_type t
      ; required = get_required t
      ; disabled = get_disabled t
      ; admin = get_admin t
      }
  ;;

  let to_entity
    Entity.Write.
      { id
      ; model
      ; name
      ; hint
      ; validation
      ; field_type
      ; required
      ; disabled
      ; admin
      }
    =
    let validation_schema schema =
      Validation.(validation |> raw_list_of_yojson |> schema)
    in
    match (field_type : FieldType.t) with
    | FieldType.Number ->
      let validation = validation_schema Validation.Number.schema in
      Number { id; model; name; hint; validation; required; disabled; admin }
    | FieldType.Text ->
      let validation = validation_schema Validation.Text.schema in
      Text { id; model; name; hint; validation; required; disabled; admin }
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
    ; answer : Repo_entity_answer.repo option
    }

  let to_entity { id; name; hint; validation; field_type; required; answer } =
    let open Public in
    let validation_schema schema =
      Validation.(validation |> raw_list_of_yojson |> schema)
    in
    match field_type with
    | FieldType.Number ->
      let answer =
        CCOption.bind answer (fun Repo_entity_answer.{ id; value; version } ->
          value
          |> CCInt.of_string
          |> CCOption.map (Entity_answer.create ~id ~version))
      in
      let validation = validation_schema Validation.Number.schema in
      Public.Number { id; name; hint; validation; required; answer }
    | FieldType.Text ->
      let answer =
        answer
        |> CCOption.map (fun Repo_entity_answer.{ id; value; version } ->
             value |> Entity_answer.create ~id ~version)
      in
      let validation = validation_schema Validation.Text.schema in
      Public.Text { id; name; hint; validation; required; answer }
  ;;

  let t =
    let encode _ =
      failwith
        Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
    in
    let decode
      (id, (name, (hint, (validation, (field_type, (required, answer))))))
      =
      Ok { id; name; hint; validation; field_type; required; answer }
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
                    (tup2 FieldType.t (tup2 Required.t (option Answer.t))))))))
  ;;
end

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
    let validation_schema schema =
      Validation.(validation |> raw_list_of_yojson |> schema)
    in
    match field_type with
    | FieldType.Number ->
      let validation = validation_schema Validation.Number.schema in
      Ok
        (Number { id; model; name; hint; validation; required; disabled; admin })
    | FieldType.Text ->
      let validation = validation_schema Validation.Text.schema in
      Ok (Text { id; model; name; hint; validation; required; disabled; admin })
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
