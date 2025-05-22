open Utils.Lwt_result.Infix

(* The API responds with access denied (403) if a specific resource is not found (middleware), and with 400 for any other error.
   As soon the API handles other requests then GET, the error handling should be adapted to the web *)
let src = Logs.Src.create "web.handler.response.api"
let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ]
let response_with_headers = Sihl.Web.Response.of_json ~headers

let respond_error ?(status = `Bad_request) ?(language = Pool_common.Language.En) error =
  let error = Pool_common.Utils.error_to_string language error in
  `Assoc [ "error", `String error ] |> response_with_headers ~status
;;

let not_found (_ : Rock.Request.t) =
  respond_error ~status:`Not_found Pool_message.(Error.NotFound Field.Resource)
  |> Lwt.return
;;

let respond ?(src = src) req result context =
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok context ->
    result context
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok result -> response_with_headers result
     | Error error -> respond_error error)
  | Error error -> respond_error ~status:`Internal_server_error error |> Lwt.return
;;

let handle ?(src = src) req result = Pool_context.Api.find req |> respond ~src req result

let handle_in_tenant_context ?(src = src) req result =
  Pool_context.find req |> respond ~src req result
;;

let index_handler
  :  query:(module Http_utils.Queryable.Queryable)
  -> ?src:Logs.src
  -> yojson_of_t:('a -> Yojson.Safe.t)
  -> Rock.Request.t
  -> (Pool_context.Api.t
      -> Guard.Actor.t
      -> Query.t
      -> ('a list * Query.t, Pool_message.Error.t) Lwt_result.t)
  -> Rock.Response.t Lwt.t
  =
  let make_meta { Query.pagination; _ } =
    pagination |> CCOption.map_or ~default:`Null Query.Pagination.yojson_of_t
  in
  fun ~query:(module Q) ?src ~yojson_of_t req run ->
    let open Utils.Lwt_result.Infix in
    let run context =
      let* actor = Pool_context.Utils.Api.find_authorizable context in
      let query =
        Query.from_request
          ?filterable_by:Q.filterable_by
          ~searchable_by:Q.searchable_by
          ~sortable_by:Q.sortable_by
          ~default:Q.default_query
          req
      in
      run context actor query
      >|+ fun (items, query) ->
      let items = `List (CCList.map yojson_of_t items) in
      let meta = make_meta query in
      `Assoc [ "data", items; "meta", meta ]
    in
    handle ?src req run
;;
