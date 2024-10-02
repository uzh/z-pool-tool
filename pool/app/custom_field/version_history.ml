open Ppx_yojson_conv_lib.Yojson_conv

module Record = struct
  include Entity

  let model = Pool_message.Field.CustomField
end

include Changelog.T (Record)
module OptionVersionHistory = Changelog.T (Entity.SelectOption)
module GroupVersionHistory = Changelog.T (Entity.Group)

module AnswerRecord = struct
  module Answer = Entity_answer
  module SelectOption = Entity.SelectOption
  module Public = Entity.Public

  type record =
    | Boolean of bool Answer.t
    | Date of Entity.Ptime.date Answer.t
    | MultiSelect of SelectOption.Id.t list Answer.t
    | Number of int Answer.t
    | Select of SelectOption.Id.t Answer.t
    | Text of string Answer.t
  [@@deriving variants, yojson]

  type t = record option

  let yojson_of_t = function
    | None -> `Null
    | Some record -> yojson_of_record record
  ;;

  let model = Pool_message.Field.CustomFieldAnswer

  let from_public (field : Public.t) : record option =
    let open CCOption in
    let select_option_id { SelectOption.Public.id; _ } = id in
    let make_record answer value admin_value =
      { Answer.id = Public.id field
      ; entity_uuid = Answer.entity_uuid answer
      ; value
      ; admin_value
      }
    in
    match field with
    | Public.Boolean (_, answer) -> map boolean answer
    | Public.Date (_, answer) -> map date answer
    | Public.MultiSelect (_, _, answer) ->
      answer
      >|= fun answer ->
      let value = Answer.value answer >|= CCList.map select_option_id in
      let admin_value =
        Answer.admin_value answer >|= CCList.map select_option_id
      in
      make_record answer value admin_value |> multiselect
    | Public.Number (_, answer) -> CCOption.map number answer
    | Public.Select (_, _, answer) ->
      answer
      >|= fun answer ->
      let value = Answer.value answer >|= select_option_id in
      let admin_value = Answer.admin_value answer >|= select_option_id in
      make_record answer value admin_value |> select
    | Public.Text (_, answer) -> CCOption.map text answer
  ;;
end

module AnswerVersionHistory = Changelog.T (AnswerRecord)
