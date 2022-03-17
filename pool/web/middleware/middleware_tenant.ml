let valid_tenant () =
  let filter handler req =
    let%lwt result =
      let open Lwt_result.Syntax in
      let* tenant_db =
        let open CCResult.Infix in
        req
        |> Pool_context.find
        >|= (fun context -> context.Pool_context.tenant_db)
        |> Lwt_result.lift
      in
      let* tenant = Pool_tenant.find_by_label tenant_db in
      match Pool_tenant.(Disabled.value tenant.disabled) with
      | false -> Lwt.return_ok tenant
      | true -> Lwt.return_error Pool_common.Message.(Disabled Tenant)
    in
    match result with
    | Ok _ -> handler req
    | Error _ -> Http_utils.redirect_to "/not-found"
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;
