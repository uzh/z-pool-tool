module Language = Pool_common.Language

let get_or_failwith = Pool_common.Utils.get_or_failwith

let tenant db_pool =
  Message_template.(
    DefaultRestored default_values_tenant |> handle_event db_pool)
;;

let root () =
  Message_template.(
    DefaultRestored default_values_root |> handle_event Pool_database.root)
;;
