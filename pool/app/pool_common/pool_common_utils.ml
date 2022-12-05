let log_src = Logs.Src.create "pool.utils"
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
           CCFormat.sprintf "(%s: %s)" k (CCString.concat ", " v))
      |> CCString.concat ", "
      |> CCFormat.sprintf "(%s)"
    in
    CCFormat.fprintf f "Schema fields: %s\n\nInput: %s" schema input
  ;;

  let decode_and_validate schema input =
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
      Logs.warn (fun m ->
        m
          "Failed to decode conformist schema: \n\n%a \n\nMessage: %s"
          pp_schema
          (schema, input)
          msg);
      result
  ;;
end

let with_log_info ?(level = info) info =
  Logs.msg level (fun m -> m "%s" (Locales_en.info_to_string info));
  info
;;

let with_log_success ?(level = info) success =
  Logs.msg level (fun m -> m "%s" (Locales_en.success_to_string success));
  success
;;

let with_log_warning ?(level = warning) warn =
  Logs.msg level (fun m -> m "%s" (Locales_en.warning_to_string warn));
  warn
;;

let with_log_error ?(level = error) ?(tags = Logs.Tag.empty) err =
  Logs.msg level (fun m ->
    m "A user experienced an error: %s" (Locales_en.error_to_string err) ~tags);
  err
;;

let with_log_result_error ~tags fcn =
  CCResult.map_err (fun err ->
    let _ = err |> fcn |> with_log_error ~tags in
    err)
;;

let decoder create_fcn field l =
  let open CCResult in
  match l with
  | x :: _ -> create_fcn x
  | [] -> Error (Entity_message.Undefined field |> with_log_error ~level:info)
;;

let schema_decoder create_fcn encode_fnc field =
  PoolConformist.custom
    (decoder create_fcn field)
    (fun l -> l |> encode_fnc |> CCList.pure)
    Entity_message.Field.(field |> show)
;;

let list_decoder create_fcn l = create_fcn l

let schema_list_decoder create_fcn encode_fnc field =
  PoolConformist.custom
    (list_decoder create_fcn)
    (fun l -> l |> encode_fnc)
    Entity_message.Field.(field |> show)
;;
