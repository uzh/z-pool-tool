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

  type 'a answer =
    { custom_field_uuid : Entity.Id.t
    ; value : 'a option
    ; admin_value : 'a option
    }
  [@@deriving show, eq, yojson]

  type record =
    | Boolean of bool answer
    | Date of Entity.Ptime.date answer
    | MultiSelect of SelectOption.Id.t list answer
    | Number of int answer
    | Select of SelectOption.Id.t answer
    | Text of string answer
  [@@deriving variants, yojson]

  type t = record option

  let yojson_of_t = function
    | None -> `Null
    | Some record ->
      let json = yojson_of_record record in
      (* Wrap the record json in an assoc, where the uuid is the key, to make
         sure the field name is visible in the changelog *)
      (match json with
       | `List [ `String _; `Assoc answer ] ->
         CCList.assoc_opt ~eq:CCString.equal "custom_field_uuid" answer
         |> (function
          | Some (`String uuid) -> `Assoc [ uuid, `Assoc answer ]
          | _ -> json)
       | _ -> json)
  ;;

  let model = Pool_message.Field.CustomFieldAnswer

  let from_public (field : Public.t) : record =
    let open CCOption.Infix in
    let select_option_id { SelectOption.Public.id; _ } = id in
    let custom_field_uuid = Public.id field in
    let make_record value admin_value =
      { custom_field_uuid; value; admin_value }
    in
    match field with
    | Public.Boolean (_, answer) ->
      let value = answer >>= Answer.value in
      let admin_value = answer >>= Answer.admin_value in
      make_record value admin_value |> boolean
    | Public.Date (_, answer) ->
      let value = answer >>= Answer.value in
      let admin_value = answer >>= Answer.admin_value in
      make_record value admin_value |> date
    | Public.MultiSelect (_, _, answer) ->
      let value = answer >>= Answer.value >|= CCList.map select_option_id in
      let admin_value =
        answer >>= Answer.admin_value >|= CCList.map select_option_id
      in
      make_record value admin_value |> multiselect
    | Public.Number (_, answer) ->
      let value = answer >>= Answer.value in
      let admin_value = answer >>= Answer.admin_value in
      make_record value admin_value |> number
    | Public.Select (_, _, answer) ->
      let value = answer >>= Answer.value >|= select_option_id in
      let admin_value = answer >>= Answer.admin_value >|= select_option_id in
      make_record value admin_value |> select
    | Public.Text (_, answer) ->
      let value = answer >>= Answer.value in
      let admin_value = answer >>= Answer.admin_value in
      make_record value admin_value |> text
  ;;
end

module AnswerVersionHistory = Changelog.T (AnswerRecord)
