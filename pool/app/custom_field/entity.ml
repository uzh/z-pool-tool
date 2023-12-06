open Ppx_yojson_conv_lib.Yojson_conv
module Message = Pool_common.Message
module Language = Pool_common.Language
module Answer = Entity_answer
module User = Pool_user

module Ptime = struct
  include Pool_common.Model.Ptime
end

let printer = Utils.ppx_printer

module Id = struct
  include Pool_common.Id
end

module Model = struct
  module Core = struct
    let field = Message.Field.Model

    type t = Contact [@name "contact"] [@printer printer "contact"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core
end

module Name = struct
  type name = string [@@deriving eq, show, yojson]

  let value_name n = n

  type t = (Language.t * name) list [@@deriving eq, show, yojson]

  let find_opt lang t = CCList.assoc_opt ~eq:Language.equal lang t
  let find_opt_or lang default t = find_opt lang t |> CCOption.value ~default

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
  module Core = struct
    let field = Message.Field.FieldType

    type t =
      | Boolean [@name "boolean"] [@printer printer "boolean"]
      | Date [@name "date"] [@printer printer "date"]
      | MultiSelect [@name "multi_select"] [@printer printer "multi_select"]
      | Number [@name "number"] [@printer printer "number"]
      | Select [@name "select"] [@printer printer "select"]
      | Text [@name "text"] [@printer printer "text"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let to_string t =
    t |> show |> CCString.replace ~sub:"_" ~by:" " |> CCString.capitalize_ascii
  ;;
end

module Required = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.Required
end

module Disabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.Disabled
end

module PublishedAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.PublishedAt create
end

module AdminHint = struct
  include Pool_common.Model.String

  let field = Message.Field.AdminHint
  let schema () = schema field ()
end

module AdminOverride = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.Override
end

module AdminViewOnly = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.AdminViewOnly
end

module AdminInputOnly = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.AdminInputOnly
end

module PromptOnRegistration = struct
  include Pool_common.Model.Boolean

  let schema = schema Message.Field.PromptOnRegistration
end

module Validation = struct
  type raw = (string * string) list [@@deriving show, eq, yojson]

  type 'a t =
    (('a -> ('a, Message.error) result) * raw
    [@equal fun (_, raw1) (_, raw2) -> equal_raw raw1 raw2])
  [@@deriving show, eq]

  let read_key key_of_yojson m =
    try Some (Utils.Json.read_variant key_of_yojson m) with
    | _ -> None
  ;;

  let build_hints read_key to_hints (rules : 'a t) =
    let open CCOption.Infix in
    rules
    |> snd
    |> CCList.filter_map (fun (key, rule_value) ->
      read_key key >>= to_hints rule_value)
  ;;

  module Text = struct
    type key =
      | TextLengthMin [@name "text_length_min"]
      [@printer printer "text_length_min"]
      | TextLengthMax [@name "text_length_max"]
      [@printer printer "text_length_max"]
    [@@deriving show, eq, yojson]

    let key_to_human = function
      | TextLengthMin -> "Text min. length"
      | TextLengthMax -> "Text max. length"
    ;;

    let read_key = read_key key_of_yojson

    let check_min_length rule_value value =
      if CCString.length value >= rule_value
      then Ok value
      else Error (Message.TextLengthMin rule_value)
    ;;

    let check_max_length rule_value value =
      if CCString.length value <= rule_value
      then Ok value
      else Error (Message.TextLengthMax rule_value)
    ;;

    let schema data =
      let open CCResult in
      ( (fun value ->
          CCList.fold_left
            (fun result (key, rule_value) ->
              let map_or = CCOption.map_or ~default:result in
              match read_key key with
              | Some TextLengthMin ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_min_length rule)
              | Some TextLengthMax ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_max_length rule)
              | None -> result)
            (Ok value)
            data)
      , data )
    ;;

    let all =
      [ show_key TextLengthMin, `Number; show_key TextLengthMax, `Number ]
    ;;

    let hints rules =
      let open CCOption.Infix in
      let open Pool_common in
      let to_hints rule_value key =
        rule_value
        |> CCInt.of_string
        >|= fun i ->
        match key with
        | TextLengthMin -> I18n.TextLengthMin i
        | TextLengthMax -> I18n.TextLengthMax i
      in
      build_hints read_key to_hints rules
    ;;
  end

  module Number = struct
    type key =
      | NumberMin [@name "number_min"] [@printer printer "number_min"]
      | NumberMax [@name "number_max"] [@printer printer "number_max"]
    [@@deriving show, eq, yojson]

    let key_to_human = function
      | NumberMin -> "Number min."
      | NumberMax -> "Number max."
    ;;

    let read_key = read_key key_of_yojson

    let check_min rule_value value =
      if value >= rule_value
      then Ok value
      else Error (Message.NumberMin rule_value)
    ;;

    let check_max rule_value value =
      if value <= rule_value
      then Ok value
      else Error (Message.NumberMax rule_value)
    ;;

    let schema data =
      let open CCResult in
      ( (fun value ->
          CCList.fold_left
            (fun result (key, rule) ->
              let map_or = CCOption.map_or ~default:result in
              match read_key key with
              | Some NumberMin ->
                rule
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_min rule)
              | Some NumberMax ->
                rule
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_max rule)
              | None -> result)
            (Ok value)
            data)
      , data )
    ;;

    let all = [ show_key NumberMin, `Number; show_key NumberMax, `Number ]

    let hints rules =
      let open CCOption.Infix in
      let open Pool_common in
      let to_hints rule_value key =
        rule_value
        |> CCInt.of_string
        >|= fun i ->
        match key with
        | NumberMin -> I18n.NumberMin i
        | NumberMax -> I18n.NumberMax i
      in
      build_hints read_key to_hints rules
    ;;
  end

  module MultiSelect = struct
    type key =
      | OptionsCountMin [@name "options_count_min"]
      [@printer printer "options_count_min"]
      | OptionsCountMax [@name "options_count_max"]
      [@printer printer "options_count_max"]
    [@@deriving show, eq, yojson]

    let key_to_human = function
      | OptionsCountMin -> "Min. number of selected options"
      | OptionsCountMax -> "Max. number of selected options"
    ;;

    let read_key = read_key key_of_yojson

    let check_options_min_count rule_value options =
      if CCList.length options >= rule_value
      then Ok options
      else Error (Message.SelectedOptionsCountMin rule_value)
    ;;

    let check_options_max_count rule_value options =
      if CCList.length options <= rule_value
      then Ok options
      else Error (Message.SelectedOptionsCountMax rule_value)
    ;;

    let schema data =
      let open CCResult in
      ( (fun value ->
          CCList.fold_left
            (fun result (key, rule_value) ->
              let map_or = CCOption.map_or ~default:result in
              match read_key key with
              | Some OptionsCountMin ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_options_min_count rule)
              | Some OptionsCountMax ->
                rule_value
                |> CCInt.of_string
                |> map_or (fun rule -> result >>= check_options_max_count rule)
              | None -> result)
            (Ok value)
            data)
      , data )
    ;;

    let all =
      [ show_key OptionsCountMin, `Number; show_key OptionsCountMax, `Number ]
    ;;

    let hints rules =
      let open CCOption.Infix in
      let open Pool_common in
      let to_hints rule_value key =
        rule_value
        |> CCInt.of_string
        >|= fun i ->
        match key with
        | OptionsCountMin -> I18n.SelectedOptionsCountMin i
        | OptionsCountMax -> I18n.SelectedOptionsCountMax i
      in
      build_hints read_key to_hints rules
    ;;
  end

  let key_to_human key =
    let open CCOption in
    let text = Text.(read_key key >|= key_to_human) in
    let number = Number.(read_key key >|= key_to_human) in
    let multi_select = MultiSelect.(read_key key >|= key_to_human) in
    CCOption.value ~default:key (text <+> number <+> multi_select)
  ;;

  let pure = CCResult.return, []
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
    go FieldType.Number Number.all
    @ go FieldType.Text Text.all
    @ go FieldType.MultiSelect MultiSelect.all
  ;;
end

module SelectOption = struct
  module Id = struct
    include Pool_common.Id
  end

  type t =
    { id : Id.t
    ; name : Name.t
    ; published_at : PublishedAt.t option
    }
  [@@deriving eq, show, yojson]

  let show_id (m : t) = m.id |> Id.value

  let name lang (t : t) =
    Name.find_opt lang t.name
    |> CCOption.to_result Message.(NotFound Field.Name)
    |> Pool_common.Utils.get_or_failwith
  ;;

  let create ?(id = Id.create ()) ?published_at name =
    { id; name; published_at }
  ;;

  let to_common_field language m =
    let name = name language m in
    Message.(Field.CustomHtmx (name, m.id |> Id.value))
  ;;

  module Public = struct
    type t =
      { id : Id.t
      ; name : Name.t
      }
    [@@deriving eq, show, yojson]

    let show_id (m : t) = m.id |> Id.value

    let name lang (t : t) =
      Name.find_opt lang t.name
      |> CCOption.to_result Message.(NotFound Field.Name)
      |> Pool_common.Utils.get_or_failwith
    ;;

    let create ?(id = Id.create ()) name = { id; name }

    let to_common_field language m =
      let name = name language m in
      Message.(Field.CustomHtmx (name, m.id |> Id.value))
    ;;
  end
end

module Public = struct
  type 'a public =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; validation : 'a Validation.t
    ; required : Required.t
    ; admin_override : AdminOverride.t
    ; admin_input_only : AdminInputOnly.t
    ; prompt_on_registration : PromptOnRegistration.t
    ; version : Pool_common.Version.t
    }
  [@@deriving eq, show]

  type t =
    | Boolean of bool public * bool Answer.t option
    | Date of Ptime.date public * Ptime.date Answer.t option
    | MultiSelect of
        SelectOption.Public.t list public
        * SelectOption.Public.t list
        * SelectOption.Public.t list Answer.t option
    | Number of int public * int Answer.t option
    | Select of
        SelectOption.Public.t public
        * SelectOption.Public.t list
        * SelectOption.Public.t Answer.t option
    | Text of string public * string Answer.t option
  [@@deriving eq, show, variants]

  let id (t : t) =
    match t with
    | Boolean ({ id; _ }, _)
    | Date ({ id; _ }, _)
    | MultiSelect ({ id; _ }, _, _)
    | Number ({ id; _ }, _)
    | Select ({ id; _ }, _, _)
    | Text ({ id; _ }, _) -> id
  ;;

  let name_value lang (t : t) =
    match t with
    | Boolean ({ name; _ }, _)
    | Date ({ name; _ }, _)
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
    | Date ({ hint; _ }, _)
    | MultiSelect ({ hint; _ }, _, _)
    | Number ({ hint; _ }, _)
    | Select ({ hint; _ }, _, _)
    | Text ({ hint; _ }, _) -> Hint.find_opt lang hint
  ;;

  let required (t : t) =
    match t with
    | Boolean ({ required; _ }, _)
    | Date ({ required; _ }, _)
    | MultiSelect ({ required; _ }, _, _)
    | Number ({ required; _ }, _)
    | Select ({ required; _ }, _, _)
    | Text ({ required; _ }, _) -> required
  ;;

  let admin_override (t : t) =
    match t with
    | Boolean ({ admin_override; _ }, _)
    | Date ({ admin_override; _ }, _)
    | MultiSelect ({ admin_override; _ }, _, _)
    | Number ({ admin_override; _ }, _)
    | Select ({ admin_override; _ }, _, _)
    | Text ({ admin_override; _ }, _) -> admin_override
  ;;

  let admin_input_only (t : t) =
    match t with
    | Boolean ({ admin_input_only; _ }, _)
    | Date ({ admin_input_only; _ }, _)
    | MultiSelect ({ admin_input_only; _ }, _, _)
    | Number ({ admin_input_only; _ }, _)
    | Select ({ admin_input_only; _ }, _, _)
    | Text ({ admin_input_only; _ }, _) -> admin_input_only
  ;;

  let prompt_on_registration (t : t) =
    match t with
    | Boolean ({ prompt_on_registration; _ }, _)
    | Date ({ prompt_on_registration; _ }, _)
    | MultiSelect ({ prompt_on_registration; _ }, _, _)
    | Number ({ prompt_on_registration; _ }, _)
    | Select ({ prompt_on_registration; _ }, _, _)
    | Text ({ prompt_on_registration; _ }, _) -> prompt_on_registration
  ;;

  let is_disabled is_admin m =
    if is_admin
    then
      (m |> admin_override |> AdminOverride.value
       || m |> admin_input_only |> AdminInputOnly.value)
      |> not
    else m |> admin_input_only |> AdminInputOnly.value
  ;;

  let version = function
    | Boolean (public, _) -> public.version
    | Date (public, _) -> public.version
    | MultiSelect (public, _, _) -> public.version
    | Number (public, _) -> public.version
    | Select (public, _, _) -> public.version
    | Text (public, _) -> public.version
  ;;

  let field_type = function
    | Boolean _ -> FieldType.Boolean
    | Date _ -> FieldType.Date
    | Number _ -> FieldType.Number
    | MultiSelect _ -> FieldType.MultiSelect
    | Select _ -> FieldType.Select
    | Text _ -> FieldType.Text
  ;;

  let increment_version t =
    let version = t |> version |> Pool_common.Version.increment in
    match t with
    | Boolean (public, answer) -> boolean { public with version } answer
    | Date (public, answer) -> date { public with version } answer
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

  let validation_hints =
    let return = CCOption.return in
    function
    | Boolean _ | Date _ | Select _ -> None
    | Number ({ validation; _ }, _) ->
      return (Validation.Number.hints validation)
    | MultiSelect ({ validation; _ }, _, _) ->
      return (Validation.MultiSelect.hints validation)
    | Text ({ validation; _ }, _) -> return (Validation.Text.hints validation)
  ;;

  let help_elements language m =
    let common_hint = to_common_hint language m in
    let validation_hints = validation_hints m |> CCOption.value ~default:[] in
    match common_hint with
    | None -> validation_hints
    | Some hint -> hint :: validation_hints
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
  ; admin_hint : AdminHint.t option
  ; admin_override : AdminOverride.t
  ; admin_view_only : AdminViewOnly.t
  ; admin_input_only : AdminInputOnly.t
  ; prompt_on_registration : PromptOnRegistration.t
  ; published_at : PublishedAt.t option
  }
[@@deriving eq, show]

type t =
  | Boolean of bool custom_field
  | Date of Ptime.date custom_field
  | Number of int custom_field
  | MultiSelect of SelectOption.t list custom_field * SelectOption.t list
  | Select of SelectOption.t custom_field * SelectOption.t list
  | Text of string custom_field
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  ?(select_options = [])
  ?published_at
  field_type
  model
  name
  hint
  validation
  required
  disabled
  custom_field_group_id
  admin_hint
  admin_override
  admin_view_only
  admin_input_only
  prompt_on_registration
  =
  let open CCResult in
  let required = if admin_input_only then false else required in
  let admin_input_only = admin_view_only || admin_input_only in
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
         ; admin_hint
         ; admin_override
         ; admin_view_only
         ; admin_input_only
         ; published_at
         ; prompt_on_registration
         })
  | FieldType.Date ->
    Ok
      (Date
         { id
         ; model
         ; name
         ; hint
         ; validation = Validation.pure
         ; required
         ; disabled
         ; custom_field_group_id
         ; admin_hint
         ; admin_override
         ; admin_view_only
         ; admin_input_only
         ; published_at
         ; prompt_on_registration
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
         ; admin_hint
         ; admin_override
         ; admin_view_only
         ; admin_input_only
         ; published_at
         ; prompt_on_registration
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
         ; admin_hint
         ; admin_override
         ; admin_view_only
         ; admin_input_only
         ; published_at
         ; prompt_on_registration
         })
  | FieldType.MultiSelect ->
    let validation = Validation.MultiSelect.schema validation in
    Ok
      (MultiSelect
         ( { id
           ; model
           ; name
           ; hint
           ; validation
           ; required
           ; disabled
           ; custom_field_group_id
           ; admin_hint
           ; admin_override
           ; admin_view_only
           ; admin_input_only
           ; published_at
           ; prompt_on_registration
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
           ; admin_hint
           ; admin_override
           ; admin_view_only
           ; admin_input_only
           ; published_at
           ; prompt_on_registration
           }
         , select_options ))
;;

let id = function
  | Boolean { id; _ }
  | Date { id; _ }
  | Number { id; _ }
  | MultiSelect ({ id; _ }, _)
  | Select ({ id; _ }, _)
  | Text { id; _ } -> id
;;

let model = function
  | Boolean { model; _ }
  | Date { model; _ }
  | Number { model; _ }
  | MultiSelect ({ model; _ }, _)
  | Select ({ model; _ }, _)
  | Text { model; _ } -> model
;;

let name = function
  | Boolean { name; _ }
  | Date { name; _ }
  | Number { name; _ }
  | MultiSelect ({ name; _ }, _)
  | Select ({ name; _ }, _)
  | Text { name; _ } -> name
;;

let name_value lang (t : t) =
  t
  |> name
  |> Name.find_opt lang
  |> CCOption.to_result Message.(NotFound Field.Name)
  |> Pool_common.Utils.get_or_failwith
;;

let hint = function
  | Boolean { hint; _ }
  | Date { hint; _ }
  | Number { hint; _ }
  | MultiSelect ({ hint; _ }, _)
  | Select ({ hint; _ }, _)
  | Text { hint; _ } -> hint
;;

let required = function
  | Boolean { required; _ }
  | Date { required; _ }
  | Number { required; _ }
  | MultiSelect ({ required; _ }, _)
  | Select ({ required; _ }, _)
  | Text { required; _ } -> required
;;

let disabled = function
  | Boolean { disabled; _ }
  | Date { disabled; _ }
  | Number { disabled; _ }
  | MultiSelect ({ disabled; _ }, _)
  | Select ({ disabled; _ }, _)
  | Text { disabled; _ } -> disabled
;;

let published_at = function
  | Boolean { published_at; _ }
  | Date { published_at; _ }
  | Number { published_at; _ }
  | MultiSelect ({ published_at; _ }, _)
  | Select ({ published_at; _ }, _)
  | Text { published_at; _ } -> published_at
;;

let group_id = function
  | Boolean { custom_field_group_id; _ }
  | Date { custom_field_group_id; _ }
  | Number { custom_field_group_id; _ }
  | MultiSelect ({ custom_field_group_id; _ }, _)
  | Select ({ custom_field_group_id; _ }, _)
  | Text { custom_field_group_id; _ } -> custom_field_group_id
;;

let admin_hint = function
  | Boolean { admin_hint; _ }
  | Date { admin_hint; _ }
  | Number { admin_hint; _ }
  | MultiSelect ({ admin_hint; _ }, _)
  | Select ({ admin_hint; _ }, _)
  | Text { admin_hint; _ } -> admin_hint
;;

let admin_override = function
  | Boolean { admin_override; _ }
  | Date { admin_override; _ }
  | Number { admin_override; _ }
  | MultiSelect ({ admin_override; _ }, _)
  | Select ({ admin_override; _ }, _)
  | Text { admin_override; _ } -> admin_override
;;

let admin_view_only = function
  | Boolean { admin_view_only; _ }
  | Date { admin_view_only; _ }
  | Number { admin_view_only; _ }
  | MultiSelect ({ admin_view_only; _ }, _)
  | Select ({ admin_view_only; _ }, _)
  | Text { admin_view_only; _ } -> admin_view_only
;;

let admin_input_only = function
  | Boolean { admin_input_only; _ }
  | Date { admin_input_only; _ }
  | Number { admin_input_only; _ }
  | MultiSelect ({ admin_input_only; _ }, _)
  | Select ({ admin_input_only; _ }, _)
  | Text { admin_input_only; _ } -> admin_input_only
;;

let prompt_on_registration = function
  | Boolean { prompt_on_registration; _ }
  | Date { prompt_on_registration; _ }
  | Number { prompt_on_registration; _ }
  | MultiSelect ({ prompt_on_registration; _ }, _)
  | Select ({ prompt_on_registration; _ }, _)
  | Text { prompt_on_registration; _ } -> prompt_on_registration
;;

let field_type = function
  | Boolean _ -> FieldType.Boolean
  | Date _ -> FieldType.Date
  | Number _ -> FieldType.Number
  | MultiSelect _ -> FieldType.MultiSelect
  | Select _ -> FieldType.Select
  | Text _ -> FieldType.Text
;;

let validation_strings =
  let open Validation in
  function
  | Boolean _ | Date _ | Select _ -> []
  | MultiSelect ({ validation; _ }, _) ->
    validation |> snd |> to_strings MultiSelect.all
  | Number { validation; _ } -> validation |> snd |> to_strings Number.all
  | Text { validation; _ } -> validation |> snd |> to_strings Text.all
;;

let validation_to_yojson = function
  | Boolean _ | Date _ | Select _ -> "[]" |> Yojson.Safe.from_string
  | MultiSelect ({ validation; _ }, _) -> Validation.encode_to_yojson validation
  | Number { validation; _ } -> Validation.encode_to_yojson validation
  | Text { validation; _ } -> Validation.encode_to_yojson validation
;;

let boolean_fields =
  Message.Field.
    [ Required
    ; Disabled
    ; Override
    ; AdminInputOnly
    ; AdminViewOnly
    ; PromptOnRegistration
    ]
;;

let has_options = function
  | MultiSelect (_, options) | Select (_, options) ->
    if CCList.is_empty options
    then Error Pool_common.Message.CustomFieldNoOptions
    else Ok ()
  | Boolean _ | Date _ | Number _ | Text _ -> Ok ()
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
    ; admin_hint : AdminHint.t option
    ; admin_override : AdminOverride.t
    ; admin_view_only : AdminViewOnly.t
    ; admin_input_only : AdminInputOnly.t
    ; prompt_on_registration : PromptOnRegistration.t
    }
  [@@deriving eq, show]
end

let group_fields groups fields =
  let partition group =
    CCList.partition
      CCFun.(
        group_id %> CCOption.map_or ~default:false (Id.equal group.Group.id))
  in
  CCList.fold_left
    (fun (grouped, ungrouped) group ->
      let of_group, ungrouped = partition group ungrouped in
      grouped @ [ group, of_group ], ungrouped)
    ([], fields)
    groups
;;

module PartialUpdate = struct
  module PoolField = Pool_common.Message.Field
  module Conformist = Pool_common.Utils.PoolConformist

  type t =
    | Firstname of Pool_common.Version.t * User.Firstname.t
    | Lastname of Pool_common.Version.t * User.Lastname.t
    | Language of Pool_common.Version.t * Pool_common.Language.t option
    | Custom of Public.t
  [@@deriving eq, show, variants]

  let is_required = function
    | Firstname _ | Lastname _ | Language _ -> true
    | Custom field -> Public.required field |> Required.value
  ;;

  let increment_version =
    let increment = Pool_common.Version.increment in
    function
    | Firstname (version, value) -> firstname (version |> increment) value
    | Lastname (version, value) -> lastname (version |> increment) value
    | Language (version, value) -> language (version |> increment) value
    | Custom custom_field -> Custom (Public.increment_version custom_field)
  ;;
end
