module Message = Pool_common.Message
module Language = Pool_common.Language
module Answer = Entity_answer

let printer m fmt _ = Format.pp_print_string fmt m

module Id = struct
  include Pool_common.Id
end

module Model = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Contact [@name "contact"] [@printer printer "contact"]
    | Experiment [@name "experiment"] [@printer printer "experiment"]
    | Session [@name "session"] [@printer printer "session"]
  [@@deriving eq, show { with_path = false }, yojson, enum]

  let field = Pool_common.Message.Field.Model

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Models: Could not create list of all models!"
  ;;

  let create s =
    try Ok (read s) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let value = show
  let schema () = Pool_common.Utils.schema_decoder create value field
end

module Name = struct
  type name = string [@@deriving eq, show, yojson]

  let value_name n = n

  type t = (Language.t * name) list [@@deriving eq, show, yojson]

  let find_opt lang t = CCList.assoc_opt ~eq:Language.equal lang t

  let create sys_languages names =
    CCList.filter
      (fun lang ->
        CCList.assoc_opt ~eq:Pool_common.Language.equal lang names
        |> CCOption.is_none)
      sys_languages
    |> function
    | [] -> Ok names
    | _ -> Error Pool_common.Message.(AllLanguagesRequired Field.Name)
  ;;
end

module Hint = struct
  type hint = string [@@deriving eq, show, yojson]

  let value_hint h = h

  type t = (Language.t * hint) list [@@deriving eq, show, yojson]

  let find_opt lang t = CCList.assoc_opt ~eq:Language.equal lang t
  let create hints = Ok hints
end

module FieldType = struct
  type t =
    | Boolean [@name "boolean"] [@printer printer "boolean"]
    | Number [@name "number"] [@printer printer "number"]
    | Select [@name "select"] [@printer printer "select"]
    | Text [@name "text"] [@printer printer "text"]
  [@@deriving eq, show { with_path = false }, yojson, enum]

  let field = Pool_common.Message.Field.FieldType

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Models: Could not create list of all models!"
  ;;

  let create s =
    try Ok (read s) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let value = show
  let schema () = Pool_common.Utils.schema_decoder create value field
end

module Required = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.Required
end

module Disabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.Disabled
end

module Admin = struct
  module Hint = struct
    include Pool_common.Model.String

    let field = Message.Field.AdminHint
    let create = create field
    let schema = schema field ?validation:None
  end

  module Overwrite = struct
    include Pool_common.Model.Boolean

    let schema = schema Pool_common.Message.Field.Overwrite
  end

  module ViewOnly = struct
    include Pool_common.Model.Boolean

    let schema = schema Pool_common.Message.Field.AdminViewOnly
  end

  module InputOnly = struct
    include Pool_common.Model.Boolean

    let schema = schema Pool_common.Message.Field.AdminInputOnly
  end

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    ; view_only : ViewOnly.t
    ; input_only : InputOnly.t
    }
  [@@deriving eq, show]

  let create hint overwrite view_only input_only =
    if view_only && not input_only
    then
      Error
        Message.(FieldRequiresCheckbox Field.(AdminViewOnly, AdminInputOnly))
    else Ok { hint; overwrite; view_only; input_only }
  ;;
end

module Validation = struct
  let printer m fmt _ = Format.pp_print_string fmt m

  type raw = (string * string) list [@@deriving show, eq, yojson]

  type 'a t =
    (('a -> ('a, Pool_common.Message.error) result) * raw
    [@equal fun (_, raw1) (_, raw2) -> equal_raw raw1 raw2])
  [@@deriving show, eq]

  module Ptime = struct
    include Ptime

    let t_of_yojson = Pool_common.Model.Ptime.t_of_yojson
    let yojson_of_t = Pool_common.Model.Ptime.yojson_of_t
  end

  module Text = struct
    let text_min_length = "text_length_min"
    let text_max_length = "text_length_max"

    let check_min_length rule_value value =
      if CCString.length value > rule_value
      then Ok value
      else Error (Message.TextLengthMin rule_value)
    ;;

    let check_max_length rule_value value =
      if CCString.length value < rule_value
      then Ok value
      else Error (Message.TextLengthMax rule_value)
    ;;

    let schema data =
      let open CCResult in
      ( (fun value ->
          CCList.fold_left
            (fun result (key, rule_value) ->
              let map_or = CCOption.map_or ~default:result in
              match key with
              | _ when CCString.equal key text_min_length ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_min_length rule)
              | _ when CCString.equal key text_max_length ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_max_length rule)
              | _ -> result)
            (Ok value)
            data)
      , data )
    ;;

    let all = [ text_min_length, `Number; text_max_length, `Number ]
  end

  module Number = struct
    let number_min = "number_min"
    let number_max = "number_max"

    let check_min rule_value value =
      if value > rule_value
      then Ok value
      else Error (Message.NumberMin rule_value)
    ;;

    let check_max rule_value value =
      if value < rule_value
      then Ok value
      else Error (Message.NumberMax rule_value)
    ;;

    let schema data =
      let open CCResult in
      ( (fun value ->
          CCList.fold_left
            (fun result (key, rule) ->
              let map_or = CCOption.map_or ~default:result in
              match key with
              | _ when CCString.equal key number_min ->
                rule
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_min rule)
              | _ when CCString.equal key number_max ->
                rule
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_max rule)
              | _ -> result)
            (Ok value)
            data)
      , data )
    ;;

    let all = [ number_min, `Number; number_max, `Number ]
  end

  let pure = CCResult.pure, []
  let encode_to_yojson t = t |> snd |> yojson_of_raw

  let to_strings all m =
    m
    |> CCList.filter_map (fun (key, value) ->
         CCList.find_opt (fun (k, _) -> CCString.equal k key) all
         |> CCOption.map (CCFun.const (key, value)))
  ;;

  let all =
    let go field_type lst =
      CCList.map (fun (key, input_type) -> key, input_type, field_type) lst
    in
    go FieldType.Number Number.all @ go FieldType.Text Text.all
  ;;
end

type 'a custom_field =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; validation : 'a Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; admin : Admin.t
  }
[@@deriving eq, show]

module SelectOption = struct
  module Id = struct
    include Pool_common.Id
  end

  type t =
    { id : Id.t
    ; name : Name.t
    }
  [@@deriving eq, show]

  let show_id (m : t) = m.id |> Id.value

  let name lang (t : t) =
    Name.find_opt lang t.name |> CCOption.get_exn_or "Cannot find field name."
  ;;

  let create ?(id = Id.create ()) name = { id; name }
end

type t =
  | Boolean of bool custom_field
  | Number of int custom_field
  | Select of SelectOption.t custom_field * SelectOption.t list
  | Text of string custom_field
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  ?(select_options = [])
  field_type
  model
  name
  hint
  validation
  required
  disabled
  admin
  =
  let open CCResult in
  match (field_type : FieldType.t) with
  | FieldType.Boolean ->
    Ok
      (Boolean
         { id
         ; model
         ; name
         ; hint
         ; validation = Validation.pure
         ; required
         ; disabled
         ; admin
         })
  | FieldType.Number ->
    let validation = Validation.Number.schema validation in
    Ok (Number { id; model; name; hint; validation; required; disabled; admin })
  | FieldType.Text ->
    let validation = Validation.Text.schema validation in
    Ok (Text { id; model; name; hint; validation; required; disabled; admin })
  | FieldType.Select ->
    Ok
      (Select
         ( { id
           ; model
           ; name
           ; hint
           ; validation = Validation.pure
           ; required
           ; disabled
           ; admin
           }
         , select_options ))
;;

module Write = struct
  type t =
    { id : Id.t
    ; model : Model.t
    ; name : Name.t
    ; hint : Hint.t
    ; validation : Yojson.Safe.t
    ; field_type : FieldType.t
    ; required : Required.t
    ; disabled : Disabled.t
    ; admin : Admin.t
    }
  [@@deriving eq, show]
end

module Public = struct
  type 'a public =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; validation : 'a Validation.t
    ; required : Required.t
    ; admin_overwrite : Admin.Overwrite.t
    ; admin_input_only : Admin.InputOnly.t
    ; answer : 'a Answer.t option
    }
  [@@deriving eq, show]

  type t =
    | Boolean of bool public
    | Number of int public
    | Select of SelectOption.t public * SelectOption.t list
    | Text of string public
  [@@deriving eq, show]

  let id (t : t) =
    match t with
    | Boolean { id; _ }
    | Number { id; _ }
    | Select ({ id; _ }, _)
    | Text { id; _ } -> id
  ;;

  let name_value lang (t : t) =
    match t with
    | Boolean { name; _ }
    | Number { name; _ }
    | Select ({ name; _ }, _)
    | Text { name; _ } ->
      Name.find_opt lang name |> CCOption.get_exn_or "Cannot find field name."
  ;;

  let hint lang (t : t) =
    match t with
    | Boolean { hint; _ }
    | Number { hint; _ }
    | Select ({ hint; _ }, _)
    | Text { hint; _ } -> Hint.find_opt lang hint
  ;;

  let required (t : t) =
    match t with
    | Boolean { required; _ }
    | Number { required; _ }
    | Select ({ required; _ }, _)
    | Text { required; _ } -> required
  ;;

  let admin_overwrite (t : t) =
    match t with
    | Boolean { admin_overwrite; _ }
    | Number { admin_overwrite; _ }
    | Select ({ admin_overwrite; _ }, _)
    | Text { admin_overwrite; _ } -> admin_overwrite
  ;;

  let admin_input_only (t : t) =
    match t with
    | Boolean { admin_input_only; _ }
    | Number { admin_input_only; _ }
    | Select ({ admin_input_only; _ }, _)
    | Text { admin_input_only; _ } -> admin_input_only
  ;;

  let version (t : t) =
    match t with
    | Boolean { answer; _ } -> answer |> CCOption.map Answer.version
    | Number { answer; _ } -> answer |> CCOption.map Answer.version
    | Select ({ answer; _ }, _) -> answer |> CCOption.map Answer.version
    | Text { answer; _ } -> answer |> CCOption.map Answer.version
  ;;

  let answer_id =
    let id a = a |> CCOption.map Answer.id in
    function
    | (Boolean { answer; _ } : t) -> id answer
    | Number { answer; _ } -> id answer
    | Select ({ answer; _ }, _) -> id answer
    | Text { answer; _ } -> id answer
  ;;

  let is_disabled is_admin m =
    if is_admin
    then m |> admin_overwrite |> Admin.Overwrite.value |> not
    else m |> admin_input_only |> Admin.InputOnly.value
  ;;

  let validate value (m : t) =
    let open CCResult.Infix in
    let go validation value = validation |> fst |> fun rule -> rule value in
    let id = answer_id m in
    let version = version m in
    match m with
    | Boolean public ->
      value
      |> Utils.Bool.of_string
      |> Answer.create ?id ?version
      |> (fun a : t -> Boolean { public with answer = a |> CCOption.pure })
      |> CCResult.pure
    | Number ({ validation; _ } as public) ->
      value
      |> CCInt.of_string
      |> CCOption.to_result Pool_common.Message.(NotANumber value)
      >>= fun i ->
      i
      |> go validation
      >|= Answer.create ?id ?version
      >|= fun a : t -> Number { public with answer = a |> CCOption.pure }
    | Select (public, options) ->
      let value = value |> SelectOption.Id.of_string in
      let selected =
        CCList.find_opt
          (fun option -> SelectOption.Id.equal option.SelectOption.id value)
          options
      in
      selected
      |> CCOption.to_result Pool_common.Message.InvalidOptionSelected
      >|= Answer.create ?id ?version
      >|= fun a : t ->
      Select ({ public with answer = a |> CCOption.pure }, options)
    | Text ({ validation; _ } as public) ->
      value
      |> go validation
      >|= Answer.create ?id ?version
      >|= fun a : t -> Text { public with answer = a |> CCOption.pure }
  ;;

  let to_common_field language m =
    let id = id m in
    let name = name_value language m in
    Pool_common.Message.(Field.CustomHtmx (name, id |> Id.value))
  ;;

  let to_common_hint language m =
    let open CCOption in
    hint language m
    >|= Hint.value_hint
    >|= fun h -> Pool_common.I18n.CustomHtmx h
  ;;
end

let id = function
  | Boolean { id; _ }
  | Number { id; _ }
  | Select ({ id; _ }, _)
  | Text { id; _ } -> id
;;

let model = function
  | Boolean { model; _ }
  | Number { model; _ }
  | Select ({ model; _ }, _)
  | Text { model; _ } -> model
;;

let name = function
  | Boolean { name; _ }
  | Number { name; _ }
  | Select ({ name; _ }, _)
  | Text { name; _ } -> name
;;

let hint = function
  | Boolean { hint; _ }
  | Number { hint; _ }
  | Select ({ hint; _ }, _)
  | Text { hint; _ } -> hint
;;

let required = function
  | Boolean { required; _ }
  | Number { required; _ }
  | Select ({ required; _ }, _)
  | Text { required; _ } -> required
;;

let disabled = function
  | Boolean { disabled; _ }
  | Number { disabled; _ }
  | Select ({ disabled; _ }, _)
  | Text { disabled; _ } -> disabled
;;

let admin = function
  | Boolean { admin; _ }
  | Number { admin; _ }
  | Select ({ admin; _ }, _)
  | Text { admin; _ } -> admin
;;

let field_type = function
  | Boolean _ -> FieldType.Boolean
  | Number _ -> FieldType.Number
  | Select _ -> FieldType.Select
  | Text _ -> FieldType.Text
;;

let validation_strings =
  let open Validation in
  function
  | Boolean _ | Select _ -> []
  | Number { validation; _ } -> validation |> snd |> to_strings Number.all
  | Text { validation; _ } -> validation |> snd |> to_strings Text.all
;;

let validation_to_yojson = function
  | Boolean _ | Select _ -> "[]" |> Yojson.Safe.from_string
  | Number { validation; _ } -> Validation.encode_to_yojson validation
  | Text { validation; _ } -> Validation.encode_to_yojson validation
;;

let boolean_fields =
  Pool_common.Message.Field.
    [ Required; Disabled; Overwrite; AdminInputOnly; AdminViewOnly ]
;;
