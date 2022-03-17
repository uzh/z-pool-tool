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
      let _ = Entity_message.((separate err).error) |> fcn |> with_log_error in
      err)
;;

let schema_decoder create_fcn err l =
  let open CCResult in
  let open Locales_en in
  let create_fcn m = create_fcn m |> CCResult.map_err error_to_string in
  match l with
  | x :: _ -> create_fcn x
  | [] ->
    Error
      (Entity_message.Undefined err
      |> with_log_error ~level:Logs.Info
      |> error_to_string)
;;
