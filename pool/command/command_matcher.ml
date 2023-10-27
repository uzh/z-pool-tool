let interval = Ptime.Span.of_int_s (5 * 60)

let run_tenant =
  Command_utils.make_pool_specific
    "matcher.run"
    "Run invitation matcher (Inverval: 5 minutes)"
    (fun pool ->
       let%lwt () = Matcher.match_invitations interval [ pool ] in
       Lwt.return_some ())
;;

let run_all =
  Command_utils.make_no_args
    "matcher.run_all"
    "Run invitation matcher on all tenants (Interval: 5 minutes)"
    (fun () ->
       let open Utils.Lwt_result.Infix in
       let%lwt () =
         Command_utils.setup_databases () >|> Matcher.match_invitations interval
       in
       Lwt.return_some ())
;;
