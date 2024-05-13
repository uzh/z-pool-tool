open CCFormat

let src = Logs.Src.create "pool.conformist"

module Error = struct
  open Pool_message.Error

  type error = t [@@deriving show, yojson]

  let invalid_bool = ConformistModuleErrorType
  let invalid_float = ConformistModuleErrorType
  let invalid_int = ConformistModuleErrorType
  let invalid_string = ConformistModuleErrorType
  let invalid_date = ConformistModuleErrorType
  let invalid_datetime = ConformistModuleErrorType
  let no_value = NoValue
  let of_string _ = ConformistModuleErrorType
end

include Conformist.Make (Error)

let pp_schema (f : formatter) (schema, input) =
  let schema =
    let f = CCFun.flip (fun field -> CCList.cons (Field.name field)) in
    fold_left ~f ~init:[] schema |> CCList.rev |> CCString.concat ", "
  in
  let input =
    input
    |> CCList.map (fun (k, v) ->
      if CCInt.equal CCString.(find ~sub:"password" (lowercase_ascii k)) (-1)
      then asprintf "(%s: %s)" k (CCString.concat ", " v)
      else asprintf "(%s: [opaque])" k)
    |> CCString.concat ", "
    |> asprintf "(%s)"
  in
  fprintf f "Schema fields: %s\n\nInput: %s" schema input
;;

let decode_and_validate ?tags schema input =
  match decode_and_validate schema input with
  | Ok _ as result -> result
  | Error errors as result ->
    let msg =
      CCList.map
        (fun (field, values, error_msg) ->
          let values = CCString.concat ", " values in
          asprintf "(%s, (%s), %s)" field values (Error.show_error error_msg))
        errors
      |> CCString.concat ", "
      |> asprintf "(%s)"
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

let decoder ?tags create_fcn field = function
  | x :: _ -> create_fcn x
  | [] ->
    let err = Pool_message.Error.Undefined field in
    Logs.msg ~src Logs.Error (fun m ->
      m ?tags "A user experienced a conformist error: %s" (Error.show_error err));
    Error err
;;

let schema_decoder ?tags ?default create_fcn encode_fnc field =
  custom
    ?default
    (decoder ?tags create_fcn field)
    CCFun.(encode_fnc %> CCList.pure)
    Pool_message.Field.(field |> show)
;;

let schema_list_decoder create_fcn encode_fnc field =
  custom create_fcn encode_fnc Pool_message.Field.(field |> show)
;;
