open CCFun
open Ppx_yojson_conv_lib.Yojson_conv
module File = Http_utils_file
module Filter = Http_utils_filter
module Message = Http_utils_message
module StringMap = CCMap.Make (CCString)
module Url = Http_utils_url

let src = Logs.Src.create "http_utils"

type json_response =
  { message : string
  ; success : bool
  }
[@@deriving yojson]

let find_intended_opt req =
  let open Uri in
  let key = Pool_message.Field.(location |> show) in
  let remove_key = CCList.filter (fun (a, _) -> CCString.equal key a |> not) in
  req
  |> Sihl.Web.Request.query key
  |> CCOption.map (fun intended ->
    Sihl.Web.Request.query_list req
    |> CCList.uniq ~eq:Utils.equal_key
    |> remove_key
    |> with_query (of_string intended)
    |> to_string)
;;

let intended_to_url url intended =
  let open Uri in
  let key = Pool_message.Field.(location |> show) in
  let equal_path a b = CCString.equal (path a) (path b) in
  let intended =
    let open CCOption in
    Sihl.Configuration.read_string "PREFIX_PATH"
    >|= (fun p ->
          if CCString.prefix ~pre:"/" p then p else Format.asprintf "/%s" p)
    |> value ~default:""
    |> fun pre ->
    CCString.chop_prefix ~pre intended |> value ~default:intended |> of_string
  in
  let url = url |> of_string in
  (if equal_path url intended then [] else [ key, [ path intended ] ])
  @ query intended
  @ query url
  |> CCList.uniq ~eq:Utils.equal_key
  |> with_query url
  |> to_string
;;

let intended_or ?default url =
  let open CCOption in
  let default = get_or ~default:url default in
  map_or ~default (intended_to_url url)
;;

let intended_of_request ({ Sihl.Web.Request.target; _ } as req) =
  match find_intended_opt req with
  | Some intended -> flip intended_to_url intended
  | None -> flip intended_to_url target
;;

let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  Pool_user.Persistence.Web.user_from_session db_pool req
;;

let get_field_router_param req field =
  Sihl.Web.Router.param req Pool_message.Field.(field |> show)
;;

let find_field_router_param_opt req field =
  try Some (get_field_router_param req field) with
  | _ -> None
;;

let find_query_lang req =
  let open CCOption.Infix in
  Sihl.Web.Request.query Pool_message.Field.(Language |> show) req
  >>= CCString.uppercase_ascii
      %> Pool_common.Language.create
      %> CCOption.of_result
;;

let find_query_param req field decode =
  let open CCResult.Infix in
  Sihl.Web.Request.query (Pool_message.Field.show field) req
  |> CCOption.to_result Pool_message.Error.(NotFound field)
  >>= decode
;;

let path_with_language lang path =
  let open Pool_common in
  let open Pool_message in
  lang
  |> CCOption.map (fun lang ->
    add_field_query_params
      path
      [ Field.Language, lang |> Language.show |> CCString.lowercase_ascii ])
  |> CCOption.value ~default:path
;;

let redirect_to_with_actions ?(skip_externalize = false) path actions =
  let externalize_path path =
    if skip_externalize then path else Sihl.Web.externalize_path path
  in
  path
  |> externalize_path
  |> Sihl.Web.Response.redirect_to
  |> CCList.fold_left ( % ) id actions
  |> Lwt.return
;;

let redirect_to path = redirect_to_with_actions path []

let set_no_cache_headers ?(enable_cache = false) res =
  if enable_cache
  then res
  else
    [ "Pragma", "no-cache"
    ; "Cache-Control", "no-cache, no-store, must-revalidate"
    ; "Expires", "0"
    ]
    |> CCList.fold_left (CCFun.flip Opium.Response.add_header) res
;;

let extract_happy_path_generic ?(src = src) ?enable_cache req result msgf =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_language; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _) -> err)
    |> CCResult.map (set_no_cache_headers ?enable_cache)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_msg, error_path) ->
      redirect_to_with_actions
        (path_with_language query_language error_path)
        [ msgf error_msg ])
  | Error err ->
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Pool_message.Error.show err));
    redirect_to "/error"
;;

let extract_happy_path ?(src = src) ?enable_cache req result =
  extract_happy_path_generic ~src ?enable_cache req result (fun err ->
    let err =
      Pool_common.Utils.with_log_error
        ~src
        ~tags:(Pool_context.Logger.Tags.req req)
        err
    in
    Message.set ~warning:[] ~success:[] ~info:[] ~error:[ err ])
;;

let extract_happy_path_with_actions ?(src = src) ?enable_cache req result =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_language; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _, _) ->
      err)
    |> CCResult.map (set_no_cache_headers ?enable_cache)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_key, error_path, error_actions) ->
      redirect_to_with_actions
        (path_with_language query_language error_path)
        (CCList.append
           [ Message.set ~warning:[] ~success:[] ~info:[] ~error:[ error_key ] ]
           error_actions))
  | Error err ->
    Logs.err ~src (fun m ->
      m ~tags "Context not found: %s" (Pool_message.Error.show err));
    redirect_to "/error"
;;

let urlencoded_to_params urlencoded keys =
  keys
  |> (CCList.map
      @@ fun key ->
      CCOption.bind (List.assoc_opt key urlencoded) CCList.head_opt
      |> CCOption.map @@ CCPair.make key)
  |> CCList.all_some
;;

let urlencoded_to_flash urlencoded =
  Sihl.Web.Flash.set
    (urlencoded
     |> CCList.map (fun (m, k) ->
       m, k |> CCList.head_opt |> CCOption.get_or ~default:""))
;;

let find_in_urlencoded_base_opt
  : string -> (string * string list) list -> string list option
  =
  CCList.assoc_opt ~eq:CCString.equal
;;

let find_in_urlencoded_list_opt field =
  let open Pool_message in
  find_in_urlencoded_base_opt Field.(array_key field)
  %> CCOption.value ~default:[]
;;

let find_in_urlencoded_opt field =
  let open Pool_message in
  find_in_urlencoded_base_opt Field.(show field)
  %> flip CCOption.bind CCList.head_opt
;;

let find_in_urlencoded ?error field =
  let open Pool_message in
  let err = CCOption.value ~default:(Error.NotFound field) error in
  find_in_urlencoded_opt field %> CCOption.to_result err
;;

(* This is required as HTMX sends "undefined" if all checkboxes are unchecked *)
let htmx_urlencoded_list key req =
  let open Utils.Lwt_result.Infix in
  Sihl.Web.Request.urlencoded_list key req
  ||> function
  | [ hd ] when CCString.equal "undefined" (hd |> CCString.lowercase_ascii) ->
    []
  | lst -> lst
;;

(* TODO[timhub]: hide information, at least on public site *)
let validate_email_existance pool email =
  let open Utils.Lwt_result.Infix in
  Pool_user.Persistence.find_by_email_opt pool email
  ||> function
  | None -> Ok ()
  | Some _ -> Error Pool_message.Error.EmailAlreadyInUse
;;

let handle_boolean_values update urlencoded values =
  let urlencoded = urlencoded |> CCList.to_seq |> StringMap.of_seq in
  CCList.fold_left update urlencoded values |> StringMap.to_seq |> CCList.of_seq
;;

let intersection_to_bool_string values =
  values
  |> CCList.inter ~eq:CCString.equal Utils.Bool.handled_true_values
  |> CCList.is_empty
  |> not
  |> string_of_bool
  |> CCList.pure
;;

let format_request_boolean_values values urlencoded =
  let update m k =
    StringMap.update
      k
      (function
        | None -> Some [ "false" ]
        | Some values -> values |> intersection_to_bool_string |> CCOption.some)
      m
  in
  handle_boolean_values update urlencoded values
;;

let format_htmx_request_boolean_values values urlencoded =
  let update m k =
    StringMap.update
      k
      (fun values -> values |> CCOption.map intersection_to_bool_string)
      m
  in
  handle_boolean_values update urlencoded values
;;

let remove_empty_values urlencoded =
  CCList.filter
    (fun (_, vs) -> CCString.concat "" vs |> CCString.equal "" |> not)
    urlencoded
;;

let remove_empty_values_multiplart urlencoded =
  CCList.filter (fun (_, vs) -> vs |> CCString.equal "" |> not) urlencoded
;;

let multipart_to_urlencoded ingnore_fields lst =
  let ingnore_fields = CCList.map Pool_message.Field.show ingnore_fields in
  CCList.filter_map
    (fun (key, value) ->
      if CCList.mem key ingnore_fields then None else Some (key, [ value ]))
    lst
;;

let placeholder_from_name = CCString.replace ~which:`All ~sub:"_" ~by:" "

let is_req_from_root_host req =
  req
  |> Sihl.Web.Request.header "host"
  |> CCOption.map2 CCString.equal_caseless Utils.Url.public_host
  |> CCOption.value ~default:false
;;

let externalize_path_with_lang lang path =
  path_with_language lang path |> Sihl.Web.externalize_path
;;

let add_line_breaks = Utils.Html.handle_line_breaks Tyxml.Html.span

let invalid_session_redirect
  ?(login_path = fun req -> "/login" |> intended_of_request req)
  req
  query_lang
  =
  redirect_to_with_actions
    (path_with_language query_lang (login_path req))
    [ Message.set ~error:[ Pool_message.Error.SessionInvalid ] ]
;;

let find_id encode field req =
  Sihl.Web.Router.param req @@ Pool_message.Field.show field |> encode
;;

let id_in_url req field =
  try find_id Pool_common.Id.of_string field req |> const true with
  | Not_found -> false
;;

let default_value_style elms =
  let open Tyxml.Html in
  div ~a:[ a_class [ "gap-sm"; "inset-sm"; "border-left" ] ] [ elms ]
;;

let externalized_path_with_version url =
  (if Sihl.Configuration.(is_development () || is_test ())
   then url
   else Format.asprintf "%s?v=%s" url Version.to_string)
  |> Sihl.Web.externalize_path
;;

let first_n_characters ?(n = 47) m =
  if CCString.length m > n
  then CCString.sub m 0 n |> Format.asprintf "%s..."
  else m
;;

module type Queryable = sig
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list
end

module Htmx = struct
  let hx_request_header = "Hx-Request"

  let is_hx_request req =
    let headers = Rock.Request.(req.headers) in
    match Httpaf.Headers.get headers hx_request_header with
    | Some "true" -> true
    | _ -> false
  ;;

  let headers =
    Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]
  ;;

  let htmx_redirect
    ?(skip_externalize = false)
    ?query_language
    ?status
    ?(actions = [])
    path
    ()
    =
    let externalize_path path =
      if skip_externalize then path else Sihl.Web.externalize_path path
    in
    Sihl.Web.Response.of_plain_text "" ?status
    |> Sihl.Web.Response.add_header
         ( "HX-Redirect"
         , path_with_language query_language path |> externalize_path )
    |> CCList.fold_left ( % ) id actions
    |> Lwt.return
  ;;

  let html_to_plain_text_response ?(status = 200) html =
    html
    |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
    |> Sihl.Web.Response.of_plain_text
         ~status:(status |> Opium.Status.of_code)
         ~headers
  ;;

  let multi_html_to_plain_text_response ?(status = 200) html_els =
    html_els
    |> CCList.fold_left
         (fun acc cur ->
           Format.asprintf "%s\n%a" acc (Tyxml.Html.pp_elt ()) cur)
         ""
    |> Sihl.Web.Response.of_plain_text
         ~status:(status |> Opium.Status.of_code)
         ~headers
  ;;

  let handler
    :  ?active_navigation:string -> error_path:string
    -> query:(module Queryable)
    -> create_layout:
         (Rock.Request.t
          -> ?active_navigation:CCString.t
          -> Pool_context.t
          -> 'page Tyxml_html.elt
          -> ( [> Html_types.html ] Tyxml_html.elt
               , Pool_message.Error.t )
               Lwt_result.t)
    -> Rock.Request.t
    -> (Pool_context.t
        -> Query.t
        -> ('page Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Response.t Lwt.t
    =
    fun ?active_navigation ~error_path ~query:(module Q) ~create_layout req run ->
    let open Utils.Lwt_result.Infix in
    extract_happy_path ~src req
    @@ fun context ->
    let query =
      Query.from_request
        ?filterable_by:Q.filterable_by
        ~searchable_by:Q.searchable_by
        ~sortable_by:Q.sortable_by
        ~default:Q.default_query
        req
    in
    let* page = run context query >|- fun err -> err, error_path in
    if is_hx_request req
    then Ok (html_to_plain_text_response page) |> Lwt_result.lift
    else
      let* view =
        create_layout ?active_navigation req context page
        >|- fun err -> err, error_path
      in
      Ok (Sihl.Web.Response.of_html view) |> Lwt_result.lift
  ;;

  let notification_id = "hx-notification"

  let notification lang ((fnc : Pool_common.Language.t -> string), classname) =
    let open Tyxml_html in
    div
      ~a:
        [ a_class [ "notification-fixed"; "fade-out" ]
        ; a_user_data "hx-swap-oob" "true"
        ; a_id notification_id
        ]
      [ div ~a:[ a_class [ "notification"; classname ] ] [ txt (fnc lang) ] ]
  ;;

  let error_notification lang err =
    let fnc lang = Pool_common.(Utils.error_to_string lang err) in
    notification lang (fnc, "error")
  ;;

  let inline_error lang err =
    let open Tyxml_html in
    div
      ~a:[ a_class [ "color-red" ] ]
      [ txt Pool_common.(Utils.error_to_string lang err) ]
  ;;

  let context_error ~src ~tags err =
    Logs.err ~src (fun m ->
      m ~tags "%s" Pool_common.(Utils.error_to_string Language.En err));
    Logs.err ~src (fun m ->
      m ~tags "Context not found: %s" (Pool_message.Error.show err));
    htmx_redirect "/error" ()
  ;;

  let handle_error_message
    ?(src = src)
    ?(error_as_notification = false)
    req
    result
    =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    match context with
    | Ok ({ Pool_context.language; _ } as context) ->
      let%lwt res = result context in
      res
      |> CCResult.get_lazy (fun error_msg ->
        let err = error_msg |> Pool_common.Utils.with_log_error in
        (fun fnc -> html_to_plain_text_response (fnc language err))
        @@ if error_as_notification then error_notification else inline_error)
      |> Lwt.return
    | Error err -> context_error ~src ~tags err
  ;;

  let extract_happy_path ?(src = src) req result =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    match context with
    | Ok ({ Pool_context.query_language; _ } as context) ->
      let%lwt res = result context in
      res
      |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _) ->
        err)
      |> CCResult.map Lwt.return
      |> CCResult.get_lazy (fun (error_msg, error_path) ->
        htmx_redirect
          error_path
          ?query_language
          ~actions:[ Message.set ~error:[ error_msg ] ]
          ())
    | Error err -> context_error ~src ~tags err
  ;;
end

module Json = struct
  let yojson_response ?status json =
    let headers =
      Opium.Headers.of_list [ "Content-Type", "application/json" ]
    in
    json |> Sihl.Web.Response.of_json ?status ~headers |> Lwt.return
  ;;

  let handle_yojson_response ?(src = src) req result =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    let return_error language err =
      yojson_response
        ~status:(Opium.Status.of_code 400)
        (`Assoc
          [ "message", `String Pool_common.(Utils.error_to_string language err)
          ])
    in
    match context with
    | Ok ({ Pool_context.language; _ } as context) ->
      let%lwt res = result context in
      res
      |> Pool_common.Utils.with_log_result_error ~src ~tags id
      |> (function
       | Ok json -> yojson_response json
       | Error error_msg -> return_error language error_msg)
    | Error err ->
      Logs.err ~src (fun m ->
        m ~tags "%s" Pool_common.(Utils.error_to_string Language.En err));
      Logs.err ~src (fun m ->
        m ~tags "Context not found: %s" (Pool_message.Error.show err));
      return_error Pool_common.Language.En err
  ;;
end
