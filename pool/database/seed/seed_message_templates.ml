module Language = Pool_common.Language

let get_or_failwith = Pool_common.Utils.get_or_failwith

let template db_pool =
  let%lwt () =
    Message_template.(
      DefaultRestored default_values_tenant |> handle_event db_pool)
  in
  Lwt.return_unit
;;
