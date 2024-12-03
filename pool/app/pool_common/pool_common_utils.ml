let src = Logs.Src.create "pool_common.utils"
let info = Logs.Info
let warning = Logs.Warning
let error = Logs.Error
let debug = Logs.Debug

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
    let (_ : Pool_message.Error.t) = err |> fcn |> with_log_error ?src ?tags in
    err)
;;
