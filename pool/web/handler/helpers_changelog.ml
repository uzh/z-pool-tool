module Response = Http_response

let src = Logs.Src.create "helpers.changelog"

let htmx_handler
  :  ?to_human:(Pool_context.t -> Changelog.t -> Changelog.t Lwt.t)
  -> url:string
  -> Pool_common.Id.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
  =
  fun ?to_human ~url entity_id req ->
  Response.Htmx.handle ~src ~error_as_notification:true req
  @@ fun ({ Pool_context.database_label; _ } as context) ->
  let open Utils.Lwt_result.Infix in
  let query =
    Query.from_request
      ?filterable_by:Changelog.filterable_by
      ~searchable_by:Changelog.searchable_by
      ~sortable_by:Changelog.sortable_by
      ~default:Changelog.default_query
      req
  in
  let%lwt changelogs =
    Changelog.all_by_entity ~query database_label entity_id
    >|> fun (changelogs, query) ->
    let%lwt changelogs =
      match to_human with
      | None -> Lwt.return changelogs
      | Some to_human -> changelogs |> Lwt_list.map_s (to_human context)
    in
    Lwt.return (changelogs, query)
  in
  let url = url |> Uri.of_string in
  Component.Changelog.list context url (Some changelogs)
  |> Response.Htmx.of_html
  |> Lwt_result.return
;;
