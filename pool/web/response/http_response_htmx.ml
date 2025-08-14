open Utils.Lwt_result.Infix
open Entity
module Page = Http_response_page

let src = Logs.Src.create "web.handler.response.htmx"
let headers = Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]

let notification lang ((fnc : Pool_common.Language.t -> string), classname) =
  let open Tyxml_html in
  div
    ~a:
      [ a_class [ "notification-fixed"; "fade-out" ]
      ; a_user_data "hx-swap-oob" "true"
      ; a_id Http_utils.Htmx.notification_id
      ]
    [ div ~a:[ a_class [ "notification"; classname ] ] [ txt (fnc lang) ] ]
;;

let error_notification lang err =
  let open Tyxml.Html in
  let html = [ p [ txt Pool_common.(Utils.error_to_string lang err) ] ] in
  Component.Notification.create lang `Error html
  |> CCList.return
  |> div
       ~a:
         [ a_class [ "notification-fixed"; "fade-out" ]
         ; a_user_data "hx-swap-oob" "true"
         ; a_id Http_utils.Htmx.notification_id
         ]
;;

let inline_error lang err =
  let open Tyxml_html in
  div ~a:[ a_class [ "color-red" ] ] [ txt Pool_common.(Utils.error_to_string lang err) ]
;;

let of_html ?(status = `OK) html =
  html
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  |> Sihl.Web.Response.of_plain_text ~status ~headers
;;

let of_html_list ?(status = `OK) html =
  html
  |> CCList.fold_left
       (fun acc cur -> Format.asprintf "%s\n%a" acc (Tyxml.Html.pp_elt ()) cur)
       ""
  |> Sihl.Web.Response.of_plain_text ~status ~headers
;;

let redirect
      ?(skip_externalize = false)
      ?(query_parameters = [])
      ?status
      ?(actions = [])
      path
  =
  let open CCFun in
  let externalize_path path =
    if skip_externalize then path else Sihl.Web.externalize_path path
  in
  Sihl.Web.Response.of_plain_text "" ?status
  |> Sihl.Web.Response.add_header
       ( "HX-Redirect"
       , Http_utils.url_with_field_params query_parameters path |> externalize_path )
  |> CCList.fold_left CCFun.( % ) id actions
  |> Lwt.return
;;

let inline_internal_server_error err =
  inline_error default_language err |> of_html ~status:`Internal_server_error
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
  let tags = Pool_context.Logger.Tags.req req in
  let result context =
    let query =
      Query.from_request
        ?filterable_by:Q.filterable_by
        ~searchable_by:Q.searchable_by
        ~sortable_by:Q.sortable_by
        ~default:Q.default_query
        req
    in
    let%lwt res = run context query in
    match res, Http_utils.Htmx.is_hx_request req with
    | Ok page, true -> of_html page |> Lwt_result.return
    | Ok page, false ->
      create_layout ?active_navigation req context page >|+ Sihl.Web.Response.of_html
    | Error err, is_hx ->
      (* Log the original error before rendering a fallback page *)
      let err = Pool_common.Utils.with_log_error ~src ~tags err in
      let page = inline_error context.Pool_context.language err in
      if is_hx
      then of_html page |> Lwt_result.return
      else create_layout ?active_navigation req context page >|+ Sihl.Web.Response.of_html
  in
  Pool_context.find req
  |> Lwt_result.lift
  >>= result
  (* Log any remaining errors (e.g., layout creation) and map to a 500 page *)
  ||> Pool_common.Utils.with_log_result_error ~src ~tags CCFun.id
  ||> function
  | Ok resp -> resp
  | Error err -> inline_internal_server_error err
;;

(** By default, htmx only handles 200 status codes

    - It would be possible to handle them manually https://htmx.org/events/#htmx:responseError
*)
let handle ?(src = src) ?(error_as_notification = false) req result =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  let log_error = Pool_common.Utils.with_log_error ~src ~tags in
  match context with
  | Ok ({ Pool_context.language; _ } as context) ->
    (match%lwt result context with
     | Ok resp -> Lwt.return resp
     | Error error ->
       let error = log_error error in
       of_html
         (if error_as_notification
          then error_notification language error
          else inline_error language error)
       |> Lwt.return)
  | Error error -> log_error error |> inline_internal_server_error |> Lwt.return
;;
