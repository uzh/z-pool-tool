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

  type t =
    | Boolean of bool answer
    | Date of Entity.Ptime.date answer
    | MultiSelect of SelectOption.Id.t list answer
    | Number of int answer
    | Select of SelectOption.Id.t answer
    | Text of string answer
  [@@deriving show, eq, variants, yojson]

  let default_record =
    let make id =
      { custom_field_uuid = id; value = None; admin_value = None }
    in
    let apply_constructor record constructor =
      make record.custom_field_uuid |> constructor
    in
    function
    | Boolean t -> apply_constructor t boolean
    | Date t -> apply_constructor t date
    | MultiSelect t -> apply_constructor t multiselect
    | Number t -> apply_constructor t number
    | Select t -> apply_constructor t select
    | Text t -> apply_constructor t text
  ;;

  let yojson_of_t record =
    let field_uuid = "custom_field_uuid" in
    let json = yojson_of_t record in
    (* Wrap the record json in an assoc, where the uuid is the key, to make sure
       the field name is visible in the changelog *)
    match json with
    | `List [ `String _; `Assoc answer ] ->
      CCList.assoc_opt ~eq:CCString.equal field_uuid answer
      |> (function
       | Some (`String uuid) ->
         let answer =
           CCList.remove_assoc ~eq:CCString.equal field_uuid answer
         in
         `Assoc [ uuid, `Assoc answer ]
       | _ -> json)
    | _ -> json
  ;;

  let model = Pool_message.Field.CustomFieldAnswer

  let from_public (field : Public.t) : t =
    let open CCOption.Infix in
    let select_option_id { SelectOption.Public.id; _ } = id in
    let custom_field_uuid = Public.id field in
    let make_record (value, admin_value) =
      { custom_field_uuid; value; admin_value }
    in
    let extract_answer answer =
      let value = answer >>= Answer.value in
      let admin_value = answer >>= Answer.admin_value in
      value, admin_value
    in
    match field with
    | Public.Boolean (_, answer) ->
      extract_answer answer |> make_record |> boolean
    | Public.Date (_, answer) -> extract_answer answer |> make_record |> date
    | Public.MultiSelect (_, _, answer) ->
      extract_answer answer
      |> CCPair.map_same (CCOption.map (CCList.map select_option_id))
      |> make_record
      |> multiselect
    | Public.Number (_, answer) ->
      extract_answer answer |> make_record |> number
    | Public.Select (_, _, answer) ->
      extract_answer answer
      |> CCPair.map_same (CCOption.map select_option_id)
      |> make_record
      |> select
    | Public.Text (_, answer) -> extract_answer answer |> make_record |> text
  ;;
end

module AnswerVersionHistory = Changelog.T (AnswerRecord)
