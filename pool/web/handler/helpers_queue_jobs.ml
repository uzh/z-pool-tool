let htmx_handler queue_table req =
  Http_utils.Htmx.handler
    ~active_navigation:(Http_utils.Url.Admin.Settings.queue_list_path `Current)
    ~error_path:"/admin"
    ~create_layout:General.create_tenant_layout
    ~query:(module Pool_queue)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt queue = Pool_queue.find_by queue_table ~query database_label in
  let open Page.Admin.Settings.Queue in
  (if HttpUtils.Htmx.is_hx_request req then list else index) queue_table context queue
  |> Lwt_result.return
;;
