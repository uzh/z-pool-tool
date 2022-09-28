Printexc.record_backtrace true

let command =
  Alcotest_lwt.
    [ ( "partial_update"
      , Partial_update.
          [ test_case "update with old version" `Quick update_with_old_version ]
      )
    ]
;;
