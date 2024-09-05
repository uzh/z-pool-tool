open CCFun

let name = "connection-watcher"
let src = Logs.Src.create [%string "system_event.%{name}"]

let verify_tenants () =
  let open Utils.Lwt_result.Infix in
  let open Database in
  let open Status in
  Logs.debug ~src (fun m ->
    m ~tags:(Logger.Tags.create root) "Check Connections");
  let actions_by_status =
    let to_connection_issue = flip Tenant.update_status ConnectionIssue in
    let to_active = flip Tenant.update_status Active in
    let clear_cache_event () =
      Entity.(Job.TenantDatabaseCacheCleared |> create)
      |> Event.(created %> handle_event)
    in
    let drop_database_event label =
      Entity.(Job.TenantDatabaseDeleted label |> create)
      |> Event.(created %> handle_event)
    in
    let add_database_event label =
      Entity.(Job.TenantDatabaseAdded label |> create)
      |> Event.(created %> handle_event)
    in
    function
    | Active ->
      ( (fun _ () -> Lwt.return_unit)
      , fun label _ ->
          let%lwt () = to_connection_issue label in
          let%lwt () = clear_cache_event () in
          drop_database_event label )
    | ConnectionIssue ->
      ( (fun label () ->
          let%lwt () = to_active label in
          let%lwt () = clear_cache_event () in
          add_database_event label)
      , fun _ _ -> Lwt.return_unit )
    | (MigrationsPending | MigrationsFailed | Disabled | Maintenance) as status
      ->
      failwith [%string "Database handling for status '%{Status.show status}'"]
  in
  let check_status status =
    let%lwt pools = Tenant.find_all_by_status ~status:[ status ] () in
    Lwt_list.iter_s
      (fun database_label ->
        let ok_fun, err_fun = actions_by_status status in
        Tenant.test_connection database_label
        >|> function
        | Ok () -> ok_fun database_label ()
        | Error err -> err_fun database_label err)
      pools
  in
  Lwt_list.iter_s check_status Status.[ ConnectionIssue; Active ]
;;
