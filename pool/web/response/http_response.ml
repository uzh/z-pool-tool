include Entity
open Utils.Lwt_result.Infix
open Pool_message
module Api = Http_response_api
module Htmx = Http_response_htmx
module Page = Http_response_page

let src = Logs.Src.create "web.handler.response"
let set_response_code status response = Rock.Response.{ response with status }
let urlencoded_remove_list = [ "password"; "csrf"; "database_url" ]

let flash_fetch_values urlencoded =
  let open CCList in
  let open CCOption in
  let apply_filter key =
    let rec compare lst =
      match lst with
      | [] -> Some key
      | sub :: tl -> if CCString.mem ~sub key then None else compare tl
    in
    compare urlencoded_remove_list
  in
  urlencoded
  |> filter_map (fun (k, v) ->
    apply_filter k >|= fun k -> k, v |> head_opt |> get_or ~default:"")
;;

let%test "filter url encoded values" =
  let urlencoded =
    [ "password", [ "Password1!" ]
    ; "new_password", [ "Password1!" ]
    ; "_csrf", [ "123456" ]
    ; "empty_list", []
    ; "multiple_values", [ "value1"; "value2" ]
    ; "foo", [ "bar" ]
    ]
    |> flash_fetch_values
  in
  let open CCList in
  let expect_none =
    [ "password"; "new_password"; "_csrf" ] |> filter (fun key -> mem_assq key urlencoded)
  in
  let expect_some =
    [ "empty_list", ""; "multiple_values", "value1"; "foo", "bar" ]
    |> filter_map (fun (key, value) ->
      match assoc_opt ~eq:CCString.equal key urlencoded with
      | None -> None
      | Some v -> if CCString.equal v value then None else Some key)
  in
  expect_none @ expect_some |> is_empty
;;

let urlencoded_to_flash_fetcher urlencoded =
  let values = flash_fetch_values urlencoded in
  fun key -> CCList.assoc_opt ~eq:CCString.equal key values
;;

let set_context_error error ({ Pool_context.message; _ } as context) =
  let open CCOption in
  let open Collection in
  message
  |> value ~default:empty
  |> add_error error
  |> return
  |> Pool_context.set_message context
;;

let set_flash_fetcher urlencoded context =
  let open CCFun.Infix in
  CCOption.map_or
    ~default:context
    (urlencoded_to_flash_fetcher %> Pool_context.set_flash_fetcher context)
    urlencoded
;;

let handle_error context req =
  let html_page status page =
    page
    |> Page.make_layout req context
    ||> Sihl.Web.Response.of_html
    ||> set_response_code status
  in
  let html_flash req handler urlencoded err code =
    let open CCFun.Infix in
    set_context_error [ err ]
    %> set_flash_fetcher urlencoded
    %> Pool_context.set req
    %> handler
    %> Lwt.map (set_response_code code)
  in
  function
  | AccessDenied -> Page.access_denied_note context |> html_page `Forbidden
  | BadRequest (handler, urlencoded, err) ->
    html_flash req handler urlencoded err `Bad_request context
  | NotFound err -> Page.not_found_note context err |> html_page `Not_found
  | Unauthorized (urlencoded, err) ->
    let open Http_utils in
    let login_path =
      (if is_req_from_root_host req then "/root/login" else "/login")
      |> intended_of_request req
    in
    redirect_to_with_actions
      login_path
      [ Message.set ~error:[ err ]
      ; Sihl.Web.Flash.set (CCOption.map_or ~default:[] flash_fetch_values urlencoded)
        (* the redirect hides the 401 status, keep it detectable for clients/monitoring *)
      ; Opium.Response.add_header ("X-Auth-Error", "unauthorized")
      ]
;;

let%test
    "unauthorized redirects to login with intended url, auth header and filtered flash"
  =
  let error = Error.LoginInvalidEmailPassword in
  let urlencoded = [ "email", [ "user@example.com" ]; "password", [ "Password1!" ] ] in
  let req = Opium.Request.get "/admin/dashboard" in
  let context =
    Pool_context.create
      ( []
      , Pool_common.Language.En
      , Database.Label.of_string "test"
      , None
      , "csrf"
      , Pool_context.Guest
      , []
      , [] )
  in
  let response =
    Lwt_main.run (handle_error context req (unauthorized ~urlencoded error))
  in
  let location =
    Opium.Response.header "Location" response
    |> CCOption.get_or ~default:""
    |> Uri.of_string
  in
  (* flash values live in the response env; read them back the way the next request
     would *)
  let flash_req = Opium.Request.get ~env:response.Opium.Response.env "/login" in
  let alert_errors =
    let open CCOption in
    Sihl.Web.Flash.find_alert flash_req
    >>= Collection.of_string
    >|= fun { Collection.error; _ } -> error
  in
  let open CCOption in
  Opium.Status.to_code response.Opium.Response.status = 302
  (* PREFIX_PATH dependent, e.g. when tests run with a loaded .env *)
  && CCString.equal (Uri.path location) (Sihl.Web.externalize_path "/login")
  && equal
       CCString.equal
       (Uri.get_query_param location "location")
       (Some "/admin/dashboard")
  && equal
       CCString.equal
       (Opium.Response.header "X-Auth-Error" response)
       (Some "unauthorized")
  && equal
       CCString.equal
       (Sihl.Web.Flash.find "email" flash_req)
       (Some "user@example.com")
  && is_none (Sihl.Web.Flash.find "password" flash_req)
  && equal (CCList.equal Error.equal) alert_errors (Some [ error ])
;;

let with_log_http_result_error ~src ~tags =
  let open CCFun in
  tap (CCResult.map_err (error_message %> Pool_common.Utils.with_log_error ~src ~tags))
;;

let bad_request_render_error context run =
  run
  >|- fun err ->
  let fallback req = Page.generic_error_response req context err in
  bad_request fallback err
;;

let generic_not_found = NotFound Pool_message.(Error.NotFound Field.Page)

let empty_not_found =
  Rock.Response.make ~status:`Not_found ~body:Rock.Body.empty ()
  |> Http_utils.set_no_cache_headers
;;

let handle ?(src = src) ?enable_cache req result =
  let open CCResult in
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok context ->
    let context =
      Pool_context.set_flash_fetcher context (CCFun.flip Sihl.Web.Flash.find req)
    in
    let%lwt res = result context in
    res
    |> with_log_http_result_error ~src ~tags
    >|= Http_utils.set_no_cache_headers ?enable_cache
    >|= Lwt.return
    |> get_lazy (handle_error context req)
  | Error err ->
    Logs.warn ~src (fun m -> m ~tags "Context not found: %s" (Error.show err));
    Page.internal_server_error_response err |> Lwt.return
;;
