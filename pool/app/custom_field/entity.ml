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

  let find_opt t lang = CCList.assoc_opt ~eq:Language.equal lang t

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

  let find_opt t lang = CCList.assoc_opt ~eq:Language.equal lang t
  let create hints = Ok hints
end

module FieldType = struct
  type t =
    | Number [@name "number"] [@printer printer "number"]
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

module Validation = struct
  module Ptime = struct
    include Ptime

    let t_of_yojson = Pool_common.Model.Ptime.t_of_yojson
    let yojson_of_t = Pool_common.Model.Ptime.yojson_of_t
  end

  module Text = struct
    type t =
      | TextLengthMin of int [@name "text_length_min"]
          [@printer printer "text_length_min"]
      | TextLengthMax of int [@name "text_length_max"]
          [@printer printer "text_length_max"]
    [@@deriving eq, show { with_path = false }, yojson, variants]

    let schema data =
      let open CCOption in
      CCList.filter_map
        (fun (key, value) ->
          match key with
          | "text_length_min" -> value |> CCInt.of_string >|= textlengthmin
          | "text_length_max" -> value |> CCInt.of_string >|= textlengthmax
          | _ -> None)
        data
    ;;

    let to_strings = function
      | TextLengthMin n -> "text_length_min", n |> CCInt.to_string
      | TextLengthMax n -> "text_length_max", n |> CCInt.to_string
    ;;

    let validate rules value =
      let open CCResult in
      let length = CCString.length value in
      let open Pool_common in
      CCList.map
        (fun rule ->
          match rule with
          | TextLengthMin i ->
            if length < i then Error (Message.TextLengthMin i) else Ok ()
          | TextLengthMax i ->
            if length > i then Error (Message.TextLengthMax i) else Ok ())
        rules
      |> CCList.all_ok
      >|= CCFun.const value
    ;;

    let all = [ "text_length_min", `Number; "text_length_max", `Number ]
  end

  module Number = struct
    type t =
      | NumberMin of int [@name "number_min"] [@printer printer "number_min"]
      | NumberMax of int [@name "number_max"] [@printer printer "number_max"]
    [@@deriving eq, show { with_path = false }, yojson, variants]

    let schema data =
      let open CCOption in
      CCList.filter_map
        (fun (key, value) ->
          match key with
          | "number_min" -> value |> CCInt.of_string >|= numbermin
          | "number_max" -> value |> CCInt.of_string >|= numbermax
          | _ -> None)
        data
    ;;

    let all = [ "number_min", `Number; "number_max", `Number ]

    let to_strings = function
      | NumberMin n -> "number_min", n |> CCInt.to_string
      | NumberMax n -> "number_max", n |> CCInt.to_string
    ;;

    let validate rules value =
      let open CCResult in
      let open Pool_common in
      CCList.map
        (fun rule ->
          match rule with
          | NumberMin i ->
            if value < i then Error (Message.NumberMin i) else Ok ()
          | NumberMax i ->
            if value > i then Error (Message.NumberMax i) else Ok ())
        rules
      |> CCList.all_ok
      >|= CCFun.const value
    ;;
  end

  type t =
    | Text of Text.t list
    | Number of Number.t list
  [@@deriving eq, show { with_path = false }, yojson, variants]

  let schema data = function
    | FieldType.Number -> data |> Number.schema |> number
    | FieldType.Text -> data |> Text.schema |> text
  ;;

  let to_strings = function
    | Text rules -> CCList.map Text.to_strings rules
    | Number rules -> CCList.map Number.to_strings rules
  ;;

  let all = [ FieldType.Text, Text.all; FieldType.Number, Number.all ]
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

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    }
  [@@deriving eq, show]
end

type t =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; field_type : FieldType.t
  ; validation : Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; admin : Admin.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  model
  name
  hint
  field_type
  validation
  required
  disabled
  admin
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
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

module Write = struct
  type t =
    { id : Id.t
    ; model : Model.t
    ; name : Name.t
    ; hint : Hint.t
    ; field_type : FieldType.t
    ; validation : Validation.t
    ; required : Required.t
    ; disabled : Disabled.t
    ; admin : Admin.t
    }
  [@@deriving eq, show]
end

module Public = struct
  type t =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; field_type : FieldType.t
    ; validation : Validation.t
    ; required : Required.t
    ; answer : Answer.t option
    }
  [@@deriving eq, show]
end

let boolean_fields = Pool_common.Message.Field.[ Required; Disabled; Overwrite ]
