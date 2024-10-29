open Utils.Lwt_result.Infix

let src = Logs.Src.create "api.api_utils"
let api_request_header = "Api-Request"

let is_api_request req =
  let headers = Rock.Request.(req.headers) in
  match Httpaf.Headers.get headers api_request_header with
  | Some "true" -> true
  | _ -> false
;;

let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ]
let response_with_headers = Sihl.Web.Response.of_json ~headers

let find_id validate_and_encode field req =
  Sihl.Web.Router.param req @@ Pool_message.Field.show field
  |> validate_and_encode
;;

let respond_error
  ?(status = `Bad_request)
  ?(language = Pool_common.Language.En)
  error
  =
  let error = Pool_common.Utils.error_to_string language error in
  `Assoc [ "error", `String error ] |> response_with_headers ~status
;;

let not_found (_ : Rock.Request.t) =
  respond_error ~status:`Not_found Pool_message.(Error.NotFound Field.Resource)
  |> Lwt.return
;;

let respond ?(src = src) req result =
  let tags = Pool_context.Logger.Tags.req req in
  let context = Pool_context.Api.find req in
  match context with
  | Ok context ->
    result context
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok result -> `Assoc [ "data", result ] |> response_with_headers
     | Error error -> respond_error error)
  | Error error ->
    respond_error ~status:`Internal_server_error error |> Lwt.return
;;

let index_handler
  :  query:(module Http_utils_queryable.Queryable) -> ?src:Logs.src
  -> yojson_of_t:('a -> Yojson.Safe.t) -> Rock.Request.t
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
    respond ?src req run
;;
