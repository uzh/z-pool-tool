let with_log_info ?(level = `Info) info =
  (match level with
  | `Debug -> Logs.debug (fun m -> m "%s" (Locales_en.info_to_string info))
  | `Info -> Logs.info (fun m -> m "%s" (Locales_en.info_to_string info))
  | `Warning -> Logs.warn (fun m -> m "%s" (Locales_en.info_to_string info))
  | `Error -> Logs.err (fun m -> m "%s" (Locales_en.info_to_string info)));
  info
;;

let with_log_success ?(level = `Info) info =
  (match level with
  | `Debug -> Logs.debug (fun m -> m "%s" (Locales_en.success_to_string info))
  | `Info -> Logs.info (fun m -> m "%s" (Locales_en.success_to_string info))
  | `Warning -> Logs.warn (fun m -> m "%s" (Locales_en.success_to_string info))
  | `Error -> Logs.err (fun m -> m "%s" (Locales_en.success_to_string info)));
  Logs.info (fun m -> m "%s" (Locales_en.success_to_string info));
  info
;;

let with_log_warning ?(level = `Warning) warn =
  (match level with
  | `Debug -> Logs.debug (fun m -> m "%s" (Locales_en.warning_to_string warn))
  | `Info -> Logs.info (fun m -> m "%s" (Locales_en.warning_to_string warn))
  | `Warning -> Logs.warn (fun m -> m "%s" (Locales_en.warning_to_string warn))
  | `Error -> Logs.err (fun m -> m "%s" (Locales_en.warning_to_string warn)));
  warn
;;

let with_log_error ?(level = `Error) err =
  (match level with
  | `Debug -> Logs.debug (fun m -> m "%s" (Locales_en.error_to_string err))
  | `Info -> Logs.info (fun m -> m "%s" (Locales_en.error_to_string err))
  | `Warning -> Logs.warn (fun m -> m "%s" (Locales_en.error_to_string err))
  | `Error -> Logs.err (fun m -> m "%s" (Locales_en.error_to_string err)));
  err
;;

let with_log_result_error fcn =
  CCResult.map_err (fun err ->
      let _ = err |> fcn |> with_log_error in
      err)
;;

let schema_decoder create_fcn err l =
  let open CCResult in
  let open Locales_en in
  let create_fcn m = create_fcn m |> CCResult.map_err error_to_string in
  l
  |> CCList.head_opt
  |> CCOption.to_result
       (Entity_message.Undefined err
       |> with_log_error ~level:`Info
       |> error_to_string)
  >>= create_fcn
;;
