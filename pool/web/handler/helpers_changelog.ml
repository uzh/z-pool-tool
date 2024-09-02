let create_layout req = General.create_tenant_layout req

let htmx_handler ~changelog ~active_navigation ~error_path ~url entity_id req =
  let open Pool_location in
  Http_utils.Htmx.handler
    ~active_navigation
    ~error_path
    ~create_layout
    ~query:changelog
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt changelogs =
    Changelog.all_by_entity ~query database_label entity_id
  in
  let url = url |> Uri.of_string in
  Component.Changelog.list context url changelogs |> Lwt_result.return
;;
