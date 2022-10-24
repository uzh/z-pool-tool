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

  let field = Message.Field.Model

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
    | _ -> Error Message.(Invalid field)
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
    | _ -> Error Message.(AllLanguagesRequired Field.Name)
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
    | MultiSelect [@name "multi_select"] [@printer printer "multi_select"]
    | Number [@name "number"] [@printer printer "number"]
    | Select [@name "select"] [@printer printer "select"]
    | Text [@name "text"] [@printer printer "text"]
  [@@deriving eq, show { with_path = false }, yojson, enum]

  let to_string t =
    t |> show |> CCString.replace ~sub:"_" ~by:" " |> CCString.capitalize_ascii
  ;;

  let field = Message.Field.FieldType

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
    | _ -> Error Message.(Invalid field)
  ;;

  let value = show
  let schema () = Pool_common.Utils.schema_decoder create value field
end

module Required = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.Required
end

module Disabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.Disabled
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

    let schema = schema Message.Field.Overwrite
  end

  module ViewOnly = struct
    include Pool_common.Model.Boolean

    let schema = schema Message.Field.AdminViewOnly
  end

  module InputOnly = struct
    include Pool_common.Model.Boolean

    let schema = schema Message.Field.AdminInputOnly
  end

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    ; view_only : ViewOnly.t
    ; input_only : InputOnly.t
    }
  [@@deriving eq, show]

  let create hint overwrite view_only input_only =
    let input_only = if view_only then true else input_only in
    Ok { hint; overwrite; view_only; input_only }
  ;;
end

module Validation = struct
  let printer m fmt _ = Format.pp_print_string fmt m

  type raw = (string * string) list [@@deriving show, eq, yojson]

  type 'a t =
    (('a -> ('a, Message.error) result) * raw
    [@equal fun (_, raw1) (_, raw2) -> equal_raw raw1 raw2])
  [@@deriving show, eq]

  module Ptime = struct
    include Pool_common.Model.Ptime
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
    Name.find_opt lang t.name
    |> CCOption.to_result Pool_common.Message.(NotFound Field.Name)
    |> Pool_common.Utils.get_or_failwith
  ;;

  let create ?(id = Id.create ()) name = { id; name }

  let to_common_field language m =
    let name = name language m in
    Pool_common.Message.(Field.CustomHtmx (name, m.id |> Id.value))
  ;;
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
    ; version : Pool_common.Version.t
    }
  [@@deriving eq, show]

  type t =
    | Boolean of bool public * bool Answer.t option
    | MultiSelect of
        SelectOption.t list public
        * SelectOption.t list
        * SelectOption.t Answer.t list
    | Number of int public * int Answer.t option
    | Select of
        SelectOption.t public
        * SelectOption.t list
        * SelectOption.t Answer.t option
    | Text of string public * string Answer.t option
  [@@deriving eq, show, variants]

  let id (t : t) =
    match t with
    | Boolean ({ id; _ }, _)
    | MultiSelect ({ id; _ }, _, _)
    | Number ({ id; _ }, _)
    | Select ({ id; _ }, _, _)
    | Text ({ id; _ }, _) -> id
  ;;

  let name_value lang (t : t) =
    match t with
    | Boolean ({ name; _ }, _)
    | MultiSelect ({ name; _ }, _, _)
    | Number ({ name; _ }, _)
    | Select ({ name; _ }, _, _)
    | Text ({ name; _ }, _) ->
      Name.find_opt lang name
      |> CCOption.to_result Pool_common.Message.(NotFound Field.Name)
      |> Pool_common.Utils.get_or_failwith
  ;;

  let hint lang (t : t) =
    match t with
    | Boolean ({ hint; _ }, _)
    | MultiSelect ({ hint; _ }, _, _)
    | Number ({ hint; _ }, _)
    | Select ({ hint; _ }, _, _)
    | Text ({ hint; _ }, _) -> Hint.find_opt lang hint
  ;;

  let required (t : t) =
    match t with
    | Boolean ({ required; _ }, _)
    | MultiSelect ({ required; _ }, _, _)
    | Number ({ required; _ }, _)
    | Select ({ required; _ }, _, _)
    | Text ({ required; _ }, _) -> required
  ;;

  let admin_overwrite (t : t) =
    match t with
    | Boolean ({ admin_overwrite; _ }, _)
    | MultiSelect ({ admin_overwrite; _ }, _, _)
    | Number ({ admin_overwrite; _ }, _)
    | Select ({ admin_overwrite; _ }, _, _)
    | Text ({ admin_overwrite; _ }, _) -> admin_overwrite
  ;;

  let admin_input_only (t : t) =
    match t with
    | Boolean ({ admin_input_only; _ }, _)
    | MultiSelect ({ admin_input_only; _ }, _, _)
    | Number ({ admin_input_only; _ }, _)
    | Select ({ admin_input_only; _ }, _, _)
    | Text ({ admin_input_only; _ }, _) -> admin_input_only
  ;;

  let is_disabled is_admin m =
    if is_admin
    then
      (m |> admin_overwrite |> Admin.Overwrite.value
      || m |> admin_input_only |> Admin.InputOnly.value)
      |> not
    else m |> admin_input_only |> Admin.InputOnly.value
  ;;

  let version = function
    | Boolean (public, _) -> public.version
    | MultiSelect (public, _, _) -> public.version
    | Number (public, _) -> public.version
    | Select (public, _, _) -> public.version
    | Text (public, _) -> public.version
  ;;

  let increment_version t =
    let version = t |> version |> Pool_common.Version.increment in
    match t with
    | Boolean (public, answer) -> boolean { public with version } answer
    | MultiSelect (public, options, answer) ->
      multiselect { public with version } options answer
    | Number (public, answer) -> number { public with version } answer
    | Select (public, options, answer) ->
      select { public with version } options answer
    | Text (public, answer) -> text { public with version } answer
  ;;

  let to_common_field language m =
    let id = id m in
    let name = name_value language m in
    Message.(Field.CustomHtmx (name, id |> Id.value))
  ;;

  let to_common_hint language m =
    let open CCOption in
    hint language m
    >|= Hint.value_hint
    >|= fun h -> Pool_common.I18n.CustomHtmx h
  ;;
end

module Group = struct
  module Id = struct
    include Pool_common.Id

    let schema = schema ~field:Message.Field.CustomFieldGroup
  end

  type t =
    { id : Id.t
    ; model : Model.t
    ; name : Name.t
    }
  [@@deriving eq, show]

  let create ?(id = Id.create ()) model name = { id; model; name }

  let name lang (t : t) =
    Name.find_opt lang t.name
    |> CCOption.to_result Pool_common.Message.(NotFound Field.Name)
    |> Pool_common.Utils.get_or_failwith
  ;;

  module Public = struct
    type t =
      { id : Id.t
      ; name : Name.t
      ; fields : Public.t list
      }
    [@@deriving eq, show]

    let name lang (t : t) =
      Name.find_opt lang t.name
      |> CCOption.to_result Pool_common.Message.(NotFound Field.Name)
      |> Pool_common.Utils.get_or_failwith
    ;;
  end
end

type 'a custom_field =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; validation : 'a Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; custom_field_group_id : Group.Id.t option
  ; admin : Admin.t
  }
[@@deriving eq, show]

type t =
  | Boolean of bool custom_field
  | Number of int custom_field
  | MultiSelect of SelectOption.t list custom_field * SelectOption.t list
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
  custom_field_group_id
  admin
  =
  let open CCResult in
  let required = if admin.Admin.input_only then false else required in
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
         ; custom_field_group_id
         ; admin
         })
  | FieldType.Number ->
    let validation = Validation.Number.schema validation in
    Ok
      (Number
         { id
         ; model
         ; name
         ; hint
         ; validation
         ; required
         ; disabled
         ; custom_field_group_id
         ; admin
         })
  | FieldType.Text ->
    let validation = Validation.Text.schema validation in
    Ok
      (Text
         { id
         ; model
         ; name
         ; hint
         ; validation
         ; required
         ; disabled
         ; custom_field_group_id
         ; admin
         })
  | FieldType.MultiSelect ->
    Ok
      (MultiSelect
         ( { id
           ; model
           ; name
           ; hint
           ; validation = Validation.pure
           ; required
           ; disabled
           ; custom_field_group_id
           ; admin
           }
         , select_options ))
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
           ; custom_field_group_id
           ; admin
           }
         , select_options ))
;;

let id = function
  | Boolean { id; _ }
  | Number { id; _ }
  | MultiSelect ({ id; _ }, _)
  | Select ({ id; _ }, _)
  | Text { id; _ } -> id
;;

let model = function
  | Boolean { model; _ }
  | Number { model; _ }
  | MultiSelect ({ model; _ }, _)
  | Select ({ model; _ }, _)
  | Text { model; _ } -> model
;;

let name = function
  | Boolean { name; _ }
  | Number { name; _ }
  | MultiSelect ({ name; _ }, _)
  | Select ({ name; _ }, _)
  | Text { name; _ } -> name
;;

let hint = function
  | Boolean { hint; _ }
  | Number { hint; _ }
  | MultiSelect ({ hint; _ }, _)
  | Select ({ hint; _ }, _)
  | Text { hint; _ } -> hint
;;

let required = function
  | Boolean { required; _ }
  | Number { required; _ }
  | MultiSelect ({ required; _ }, _)
  | Select ({ required; _ }, _)
  | Text { required; _ } -> required
;;

let disabled = function
  | Boolean { disabled; _ }
  | Number { disabled; _ }
  | MultiSelect ({ disabled; _ }, _)
  | Select ({ disabled; _ }, _)
  | Text { disabled; _ } -> disabled
;;

let group_id = function
  | Boolean { custom_field_group_id; _ }
  | Number { custom_field_group_id; _ }
  | MultiSelect ({ custom_field_group_id; _ }, _)
  | Select ({ custom_field_group_id; _ }, _)
  | Text { custom_field_group_id; _ } -> custom_field_group_id
;;

let admin = function
  | Boolean { admin; _ }
  | Number { admin; _ }
  | MultiSelect ({ admin; _ }, _)
  | Select ({ admin; _ }, _)
  | Text { admin; _ } -> admin
;;

let field_type = function
  | Boolean _ -> FieldType.Boolean
  | Number _ -> FieldType.Number
  | MultiSelect _ -> FieldType.MultiSelect
  | Select _ -> FieldType.Select
  | Text _ -> FieldType.Text
;;

let validation_strings =
  let open Validation in
  function
  | Boolean _ | Select _ | MultiSelect _ -> []
  | Number { validation; _ } -> validation |> snd |> to_strings Number.all
  | Text { validation; _ } -> validation |> snd |> to_strings Text.all
;;

let validation_to_yojson = function
  | Boolean _ | Select _ | MultiSelect _ -> "[]" |> Yojson.Safe.from_string
  | Number { validation; _ } -> Validation.encode_to_yojson validation
  | Text { validation; _ } -> Validation.encode_to_yojson validation
;;

let boolean_fields =
  Message.Field.[ Required; Disabled; Overwrite; AdminInputOnly; AdminViewOnly ]
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
    ; custom_field_group_id : Group.Id.t option
    ; admin : Admin.t
    }
  [@@deriving eq, show]
end
