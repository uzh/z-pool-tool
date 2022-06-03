module Language = Pool_common.Language

let get_or_failwith = Pool_common.Utils.get_or_failwith

let i18n db_pool =
  let%lwt () = I18n.(DefaultRestored default_values |> handle_event db_pool) in
  Lwt.return_unit
;;
