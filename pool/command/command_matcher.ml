let run_tenant =
  Command_utils.make_pool_specific
    "matcher.run"
    "Run invitation matcher"
    (fun pool ->
    let%lwt () = Matcher.match_invitations [ pool ] in
    Lwt.return_some ())
;;

let run_all =
  Command_utils.make_no_args
    "matcher.run_all"
    "Run invitation matcher on all tenants"
    (fun () ->
    let open Utils.Lwt_result.Infix in
    let%lwt () =
      Command_utils.setup_databases () >|> Matcher.match_invitations
    in
    Lwt.return_some ())
;;
