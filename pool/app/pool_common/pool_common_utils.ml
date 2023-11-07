let src = Logs.Src.create "pool_common.utils"
let info = Logs.Info
let warning = Logs.Warning
let error = Logs.Error
let debug = Logs.Debug

module Time = Utils_time

module Error = struct
  type error = Entity_message.error [@@deriving eq, show, yojson]

  let invalid_bool = Entity_message.(ConformistModuleErrorType)
  let invalid_float = Entity_message.(ConformistModuleErrorType)
  let invalid_int = Entity_message.(ConformistModuleErrorType)
  let invalid_string = Entity_message.(ConformistModuleErrorType)
  let invalid_date = Entity_message.(ConformistModuleErrorType)
  let invalid_datetime = Entity_message.(ConformistModuleErrorType)
  let no_value = Entity_message.(NoValue)
  let of_string _ = Entity_message.(ConformistModuleErrorType)
end

module PoolConformist = struct
  include Conformist.Make (Error)

  let pp_schema (f : CCFormat.formatter) (schema, input) =
    let schema =
      fold_left
        ~f:(fun res field -> CCList.cons (Field.name field) res)
        ~init:[]
        schema
      |> CCList.rev
      |> CCString.concat ", "
    in
    let input =
      input
      |> CCList.map (fun (k, v) ->
        if CCInt.equal CCString.(find ~sub:"password" (lowercase_ascii k)) (-1)
        then CCFormat.sprintf "(%s: %s)" k (CCString.concat ", " v)
        else CCFormat.sprintf "(%s: [opaque])" k)
      |> CCString.concat ", "
      |> CCFormat.sprintf "(%s)"
    in
    CCFormat.fprintf f "Schema fields: %s\n\nInput: %s" schema input
  ;;

  let decode_and_validate ?tags schema input =
    let result = decode_and_validate schema input in
    match result with
    | Ok _ as result -> result
    | Error errors as result ->
      let msg =
        CCList.map
          (fun (field, values, error_msg) ->
            let values = CCString.concat ", " values in
            CCFormat.sprintf
              "(%s, (%s), %s)"
              field
              values
              (Error.show_error error_msg))
          errors
        |> CCString.concat ", "
        |> CCFormat.sprintf "(%s)"
      in
      Logs.warn ~src (fun m ->
        m
          ?tags
          "Failed to decode conformist schema: \n\n%a \n\nMessage: %s"
          pp_schema
          (schema, input)
          msg);
      result
  ;;
end

let handle_ppx_yojson_err (exn, yojson) =
  let msg =
    Format.asprintf
      "Yojson_conv error: %s\n\nAffected yojson: %s"
      (Printexc.to_string exn)
      ([%show: Yojson.Safe.t] yojson)
  in
  Error Entity_message.(NotHandled msg)
;;

let handle_json_parse_err str =
  let msg = Format.asprintf "Json parse error: %s" str in
  Error Entity_message.(InvalidJson msg)
;;

let with_log_info ?(src = src) ?(tags = Logs.Tag.empty) ?(level = info) info =
  Logs.msg ~src level (fun m -> m ~tags "%s" (Locales_en.info_to_string info));
  info
;;

let with_log_success
  ?(src = src)
  ?(tags = Logs.Tag.empty)
  ?(level = info)
  success
  =
  Logs.msg ~src level (fun m ->
    m ~tags "%s" (Locales_en.success_to_string success));
  success
;;

let with_log_warning
  ?(src = src)
  ?(tags = Logs.Tag.empty)
  ?(level = warning)
  warn
  =
  Logs.msg ~src level (fun m ->
    m ~tags "%s" (Locales_en.warning_to_string warn));
  warn
;;

let with_log_error ?(src = src) ?(tags = Logs.Tag.empty) ?(level = error) err =
  Logs.msg ~src level (fun m ->
    m ~tags "A user experienced an error: %s" (Locales_en.error_to_string err));
  err
;;

let with_log_result_error ?src ?tags fcn =
  CCResult.map_err (fun err ->
    let (_ : Entity_message.error) = err |> fcn |> with_log_error ?src ?tags in
    err)
;;

let decoder ?tags create_fcn field = function
  | x :: _ -> create_fcn x
  | [] ->
    Error (Entity_message.Undefined field |> with_log_error ?tags ~level:info)
;;

let schema_decoder ?tags ?default create_fcn encode_fnc field =
  PoolConformist.custom
    ?default
    (decoder ?tags create_fcn field)
    CCFun.(encode_fnc %> CCList.pure)
    Entity_message.Field.(field |> show)
;;

let schema_list_decoder create_fcn encode_fnc field =
  PoolConformist.custom
    create_fcn
    encode_fnc
    Entity_message.Field.(field |> show)
;;
