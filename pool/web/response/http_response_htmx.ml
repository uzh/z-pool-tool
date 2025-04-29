open Utils.Lwt_result.Infix
module Page = Http_response_page

let src = Logs.Src.create "web.handler.response.htmx"
let headers = Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]

let html_to_plain_text_response ?(status = `OK) html =
  html
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  |> Sihl.Web.Response.of_plain_text ~status ~headers
;;

let html_list_to_plain_text_response ?(status = `OK) html =
  html
  |> CCList.fold_left
       (fun acc cur -> Format.asprintf "%s\n%a" acc (Tyxml.Html.pp_elt ()) cur)
       ""
  |> Sihl.Web.Response.of_plain_text ~status ~headers
;;

let index_handler
  :  ?active_navigation:string
  -> query:(module Http_utils.Queryable.Queryable)
  -> create_layout:
       (Rock.Request.t
        -> ?active_navigation:CCString.t
        -> Pool_context.t
        -> 'page Tyxml_html.elt
        -> ([> Html_types.html ] Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t)
  -> Rock.Request.t
  -> (Pool_context.t
      -> Query.t
      -> ('page Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t)
  -> Rock.Response.t Lwt.t
  =
  fun ?active_navigation ~query:(module Q) ~create_layout req run ->
  let result context =
    let query =
      Query.from_request
        ?filterable_by:Q.filterable_by
        ~searchable_by:Q.searchable_by
        ~sortable_by:Q.sortable_by
        ~default:Q.default_query
        req
    in
    let%lwt page =
      run context query
      ||> CCResult.get_lazy (fun error ->
        Page.htmx_onpage_error_note context.Pool_context.language error)
    in
    match Http_utils.Htmx.is_hx_request req with
    | true -> Http_utils.Htmx.html_to_plain_text_response page |> Lwt_result.return
    | false ->
      create_layout ?active_navigation req context page >|+ Sihl.Web.Response.of_html
  in
  let tags = Pool_context.Logger.Tags.req req in
  Pool_context.find req
  |> Lwt_result.lift
  >>= result
  ||> Pool_common.Utils.with_log_result_error ~src ~tags CCFun.id
  ||> CCResult.get_lazy Page.internal_server_error_response
;;
