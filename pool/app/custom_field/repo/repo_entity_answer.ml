open Entity_answer
module Base = Repo_entity_base
module Common = Pool_common
module Repo = Common.Repo

module Value = struct
  let t = Caqti_type.string
end

module Write = struct
  type t =
    { id : Id.t
    ; custom_field_uuid : Id.t
    ; entity_uuid : Id.t
    ; value : string
    ; version : Pool_common.Version.t
    }
  [@@deriving show, eq]

  let of_entity id custom_field_uuid entity_uuid value version =
    { id; custom_field_uuid; entity_uuid; value; version }
  ;;

  let t =
    let encode (m : t) =
      Ok (m.id, (m.custom_field_uuid, (m.entity_uuid, (m.value, m.version))))
    in
    let decode (id, (custom_field_uuid, (entity_uuid, (value, version)))) =
      Ok { id; custom_field_uuid; entity_uuid; value; version }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2 Repo.Id.t (t2 Repo.Id.t (t2 Repo.Id.t (t2 Value.t Repo.Version.t)))))
  ;;
end

module Override = struct
  type t =
    { custom_field_uuid : Id.t
    ; entity_uuid : Id.t
    ; value : string option
    ; admin_value : string option
    ; version : Pool_common.Version.t
    }
  [@@deriving show, eq]

  let of_entity custom_field_uuid entity_uuid value admin_value version =
    { custom_field_uuid; entity_uuid; value; admin_value; version }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.custom_field_uuid
        , (m.entity_uuid, (m.value, (m.admin_value, m.version))) )
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Repo.Id.t
           (t2
              Repo.Id.t
              (t2 (option Value.t) (t2 (option Value.t) Repo.Version.t)))))
  ;;
end

module VersionHistory = struct
  module FieldType = Entity.FieldType
  module SelectOption = Entity.SelectOption

  type repo = string t

  open Version_history.AnswerRecord

  let t =
    let ( %> ) = CCFun.( %> ) in
    let encode _ =
      Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel
    in
    let decode (field_type, custom_field_uuid, value, admin_value) =
      let make_anwer decode =
        let decode_opt value = CCOption.bind value decode in
        let value = decode_opt value in
        let admin_value = decode_opt admin_value in
        { custom_field_uuid; value; admin_value }
      in
      CCResult.return
      @@
      match field_type with
      | FieldType.Boolean ->
        make_anwer (Utils.Bool.of_string %> CCOption.return) |> boolean
      | FieldType.Date ->
        make_anwer (Entity.Ptime.date_of_string %> CCResult.to_opt) |> date
      | FieldType.Number -> make_anwer CCInt.of_string |> number
      | FieldType.Select ->
        make_anwer (SelectOption.Id.of_string %> CCOption.return) |> select
      | FieldType.MultiSelect ->
        let decode value =
          try
            value
            |> Yojson.Safe.from_string
            |> Base.multi_select_answer_of_yojson
            |> CCOption.return
          with
          | _ -> None
        in
        make_anwer decode |> multiselect
      | FieldType.Text -> make_anwer CCOption.return |> text
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t4 Base.FieldType.t Repo.Id.t (option Value.t) (option Value.t)))
  ;;
end
