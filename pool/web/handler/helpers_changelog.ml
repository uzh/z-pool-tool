let src = Logs.Src.create "helpers.changelog"

let htmx_handler
  :  version_history:(module Changelog.TSig)
  -> ?to_human:(Pool_context.t -> Changelog.t -> Changelog.t Lwt.t)
  -> url:string -> Pool_common.Id.t -> Rock.Request.t -> Rock.Response.t Lwt.t
  =
  fun ~version_history:(module Q) ?to_human ~url entity_id req ->
  Http_utils.Htmx.handle_error_message ~src ~error_as_notification:true req
  @@ fun ({ Pool_context.database_label; _ } as context) ->
  let open Utils.Lwt_result.Infix in
  let () = Logs.info (fun m -> m "Changelog handler") in
  let query =
    Query.from_request
      ?filterable_by:Q.filterable_by
      ~searchable_by:Q.searchable_by
      ~sortable_by:Q.sortable_by
      ~default:Q.default_query
      req
  in
  let%lwt changelogs =
    Q.all_by_entity ~query database_label entity_id
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
  |> Http_utils.Htmx.html_to_plain_text_response
  |> Lwt_result.return
;;
