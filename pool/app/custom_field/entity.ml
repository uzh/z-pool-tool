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
    |> CCOption.get_exn_or "Models: Could not create list of all\nmodels!"
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

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    }
  [@@deriving eq, show]
end

module Validation = struct
  let printer m fmt _ = Format.pp_print_string fmt m

  type raw = string * string [@@deriving show, eq, yojson]
  type raw_list = raw list [@@deriving show, eq, yojson]

  module Ptime = struct
    include Ptime

    let t_of_yojson = Pool_common.Model.Ptime.t_of_yojson
    let yojson_of_t = Pool_common.Model.Ptime.yojson_of_t
  end

  module Text = struct
    let text_min_length = "text_length_min"
    let text_max_length = "text_length_max"

    let schema data =
      let open CCOption in
      CCList.filter_map
        (fun (key, value) ->
          (match key with
           | _ when CCString.equal key text_min_length ->
             value
             |> CCInt.of_string
             >|= fun min str ->
             if CCString.length str < min
             then Error (Message.TextLengthMin min)
             else Ok str
           | _ when CCString.equal key text_max_length ->
             value
             |> CCInt.of_string
             >|= fun max str ->
             if CCString.length str > max
             then Error (Message.TextLengthMin max)
             else Ok str
           | _ -> None)
          |> CCOption.map (fun r -> r, (key, value)))
        data
    ;;

    let all = [ text_min_length, `Number; text_max_length, `Number ]
  end

  module Number = struct
    let number_min = "number_min"
    let number_max = "number_max"

    let schema data =
      let open CCOption in
      CCList.filter_map
        (fun (key, value) ->
          (match key with
           | _ when CCString.equal key number_min ->
             value
             |> CCInt.of_string
             >|= fun min i ->
             if i < min then Error (Message.NumberMin min) else Ok i
           | _ when CCString.equal key number_max ->
             value
             |> CCInt.of_string
             >|= fun max i ->
             if i > max then Error (Message.NumberMax max) else Ok i
           | _ -> None)
          |> CCOption.map (fun r -> r, (key, value)))
        data
    ;;

    let all = [ number_min, `Number; number_max, `Number ]
  end

  let encode_to_yojson t =
    t |> CCList.map (fun (_, raw) -> raw) |> yojson_of_raw_list
  ;;

  let to_strings all m =
    m
    |> CCList.filter_map (fun (_, (key, value)) ->
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

type 'a validation =
  (('a -> ('a, Pool_common.Message.error) result) * Validation.raw
  [@equal fun (_, raw1) (_, raw2) -> Validation.equal_raw raw1 raw2])
[@@deriving show, eq]

type 'a custom_field =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; validation : 'a validation list
  ; required : Required.t
  ; disabled : Disabled.t
  ; admin : Admin.t
  }
[@@deriving eq, show]

type t =
  | Number of int custom_field
  | Text of string custom_field
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
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
  | FieldType.Number ->
    let validation = Validation.Number.schema validation in
    Ok (Number { id; model; name; hint; validation; required; disabled; admin })
  | FieldType.Text ->
    let validation = Validation.Text.schema validation in
    Ok (Text { id; model; name; hint; validation; required; disabled; admin })
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
    ; validation : 'a validation list
    ; required : Required.t
    ; answer : 'a Answer.t option
    }
  [@@deriving eq, show]

  type t =
    | Number of int public
    | Text of string public
  [@@deriving eq, show]

  let validate value (m : t) =
    let open CCResult.Infix in
    let go rules value =
      CCList.fold_left
        (fun result (rule, _) -> result >>= rule)
        (Ok value)
        rules
    in
    match m with
    | Number ({ validation; answer; _ } as public) ->
      let id = answer |> CCOption.map Answer.id in
      let version = answer |> CCOption.map Answer.version in
      value
      |> CCInt.of_string
      |> CCOption.to_result Pool_common.Message.(NotANumber value)
      >>= fun i ->
      i
      |> go validation
      >|= Answer.create ?id ?version
      >|= fun a : t -> Number { public with answer = a |> CCOption.pure }
    | Text ({ validation; answer; _ } as public) ->
      let id = answer |> CCOption.map Answer.id in
      let version = answer |> CCOption.map Answer.version in
      value
      |> go validation
      >|= Answer.create ?id ?version
      >|= fun a : t -> Text { public with answer = a |> CCOption.pure }
  ;;

  let get_id (t : t) =
    match t with
    | Number { id; _ } | Text { id; _ } -> id
  ;;

  let get_name lang (t : t) =
    match t with
    | Number { name; _ } | Text { name; _ } -> Name.find_opt lang name
  ;;

  let get_hint lang (t : t) =
    match t with
    | Number { hint; _ } | Text { hint; _ } -> Hint.find_opt lang hint
  ;;

  let get_version (t : t) =
    match t with
    | Number { answer; _ } -> answer |> CCOption.map Answer.version
    | Text { answer; _ } -> answer |> CCOption.map Answer.version
  ;;
end

let get_id = function
  | Number { id; _ } | Text { id; _ } -> id
;;

let get_model = function
  | Number { model; _ } | Text { model; _ } -> model
;;

let get_name = function
  | Number { name; _ } | Text { name; _ } -> name
;;

let get_hint = function
  | Number { hint; _ } | Text { hint; _ } -> hint
;;

let get_required = function
  | Number { required; _ } | Text { required; _ } -> required
;;

let get_disabled = function
  | Number { disabled; _ } | Text { disabled; _ } -> disabled
;;

let get_admin = function
  | Number { admin; _ } | Text { admin; _ } -> admin
;;

let get_field_type = function
  | Number _ -> FieldType.Number
  | Text _ -> FieldType.Text
;;

let get_validation_strings =
  let open Validation in
  function
  | Number { validation; _ } -> validation |> to_strings Number.all
  | Text { validation; _ } -> validation |> to_strings Text.all
;;

(* TODO: Is this needed? *)
let validation_to_yojson = function
  | Number { validation; _ } -> Validation.encode_to_yojson validation
  | Text { validation; _ } -> Validation.encode_to_yojson validation
;;

let boolean_fields = Pool_common.Message.Field.[ Required; Disabled; Overwrite ]
