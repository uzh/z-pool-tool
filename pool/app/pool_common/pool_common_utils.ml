module Error = struct
  type error = Entity_message.error [@@deriving eq, show, yojson]

  (* TODO: create dummy errors *)

  let invalid_bool = Entity_message.(Invalid Key)
  let invalid_float = Entity_message.(Invalid Key)
  let invalid_int = Entity_message.(Invalid Key)
  let invalid_string = Entity_message.(Invalid Key)
  let invalid_date = Entity_message.(Invalid Key)
  let invalid_datetime = Entity_message.(Invalid Key)
  let no_value = Entity_message.(Invalid Key)
  let of_string _ = Entity_message.(Invalid Key)
end

module PoolConformist = Conformist.Make (Error)

let with_log_info ?(level = Logs.Info) info =
  Logs.msg level (fun m -> m "%s" (Locales_en.info_to_string info));
  info
;;

let with_log_success ?(level = Logs.Info) success =
  Logs.msg level (fun m -> m "%s" (Locales_en.success_to_string success));
  success
;;

let with_log_warning ?(level = Logs.Warning) warn =
  Logs.msg level (fun m -> m "%s" (Locales_en.warning_to_string warn));
  warn
;;

let with_log_error ?(level = Logs.Error) err =
  Logs.msg level (fun m -> m "%s" (Locales_en.error_to_string err));
  err
;;

let with_log_result_error fcn =
  CCResult.map_err (fun err ->
      let _ = err |> fcn |> with_log_error in
      err)
;;

let decoder create_fcn field l =
  let open CCResult in
  match l with
  | x :: _ -> create_fcn x
  | [] ->
    Error (Entity_message.Undefined field |> with_log_error ~level:Logs.Info)
;;

let schema_decoder create_fcn encode_fnc field field_string =
  PoolConformist.custom
    (decoder create_fcn field)
    (fun l -> l |> encode_fnc |> CCList.pure)
    field_string
;;

let list_decoder create_fcn l = create_fcn l

let schema_list_decoder create_fcn encode_fnc field_string =
  PoolConformist.custom
    (list_decoder create_fcn)
    (fun l -> l |> encode_fnc)
    field_string
;;
