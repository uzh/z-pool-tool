let valid_tenant () =
  let filter handler req =
    let%lwt result =
      let open Utils.Lwt_result.Infix in
      let* tenant_db =
        let open CCResult.Infix in
        req
        |> Pool_context.find
        >|= (fun { Pool_context.tenant_db; _ } -> tenant_db)
        |> Lwt_result.lift
      in
      let* tenant = Pool_tenant.find_by_label tenant_db in
      let%lwt tenant_languages = Settings.find_languages tenant_db in
      match Pool_tenant.(Disabled.value tenant.disabled) with
      | false ->
        Lwt.return_ok (Pool_context.Tenant.create tenant tenant_languages)
      | true -> Lwt.return_error Pool_common.Message.(Disabled Field.Tenant)
    in
    match result with
    | Ok context -> context |> Pool_context.Tenant.set req |> handler
    | Error _ -> Http_utils.redirect_to "/not-found"
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;
