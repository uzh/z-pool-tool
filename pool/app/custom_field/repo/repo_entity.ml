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
    let encode = encode_yojson yojson_of_t in
    let decode =
      decode_yojson t_of_yojson Pool_common.Message.Field.Validation
    in
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
      { id = t.id
      ; model = t.model
      ; name = t.name
      ; hint = t.hint
      ; field_type = t.field_type
      ; validation = t.validation
      ; required = t.required
      ; disabled = t.disabled
      ; admin = t.admin
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
  let t =
    let open Public in
    let encode m =
      Ok
        ( m.Public.id
        , ( m.name
          , (m.hint, (m.field_type, (m.validation, (m.required, m.answer)))) )
        )
    in
    let decode
      (id, (name, (hint, (field_type, (validation, (required, answer))))))
      =
      Ok { id; name; hint; field_type; validation; required; answer }
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
                    FieldType.t
                    (tup2 Validation.t (tup2 Required.t (option Answer.t))))))))
  ;;
end

let t =
  let encode m =
    Ok
      ( m.id
      , ( m.model
        , ( m.name
          , ( m.hint
            , ( m.field_type
              , ( m.validation
                , ( m.required
                  , (m.disabled, (m.admin, (m.created_at, m.updated_at))) ) ) )
            ) ) ) )
  in
  let decode
    ( id
    , ( model
      , ( name
        , ( hint
          , ( field_type
            , ( validation
              , (required, (disabled, (admin, (created_at, updated_at)))) ) ) )
        ) ) )
    =
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
      ; created_at
      ; updated_at
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
                                 Admin.t
                                 (tup2
                                    Common.Repo.CreatedAt.t
                                    Common.Repo.UpdatedAt.t)))))))))))
;;
