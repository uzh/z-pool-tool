open CCFun
open Ppx_yojson_conv_lib.Yojson_conv
module Api = Http_utils_api
module File = Http_utils_file
module Filter = Http_utils_filter
module Message = Http_utils_message
module Queryable = Http_utils_queryable
module Session = Http_utils_session
module StringMap = CCMap.Make (CCString)
module Url = Http_utils_url

let src = Logs.Src.create "http_utils"

type json_response =
  { message : string
  ; success : bool
  }
[@@deriving yojson]

let url_with_field_params path params = Pool_message.add_field_query_params params path

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
    >|= (fun p -> if CCString.prefix ~pre:"/" p then p else Format.asprintf "/%s" p)
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

let user_from_session db_pool req : Pool_user.t option Lwt.t =
  Pool_user.Web.user_from_session db_pool req
;;

let get_field_router_param req field =
  Sihl.Web.Router.param req Pool_message.Field.(field |> show)
;;

let find_field_router_param_opt req field =
  try Some (get_field_router_param req field) with
  | _ -> None
;;

let retain_url_params req url =
  let open Uri in
  req.Opium.Request.target |> of_string |> query |> with_query (of_string url)
;;

let find_query_lang req =
  let open CCOption.Infix in
  Sihl.Web.Request.query Pool_message.Field.(Language |> show) req
  >>= CCString.uppercase_ascii %> Pool_common.Language.create %> CCOption.of_result
;;

let find_query_param req field decode =
  let open CCResult.Infix in
  Sihl.Web.Request.query (Pool_message.Field.show field) req
  |> CCOption.to_result Pool_message.Error.(NotFound field)
  >>= decode
;;

let find_referer req =
  let open CCOption.Infix in
  Httpaf.Headers.get req.Opium.Request.headers "referer" >|= Uri.of_string >|= Uri.path
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

let redirect_back ~fallback req actions =
  let redirect = find_referer req |> CCOption.value ~default:fallback in
  redirect_to_with_actions redirect actions
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
     |> CCList.map (fun (m, k) -> m, k |> CCList.head_opt |> CCOption.get_or ~default:"")
    )
;;

let find_in_urlencoded_base_opt
  : string -> (string * string list) list -> string list option
  =
  CCList.assoc_opt ~eq:CCString.equal
;;

let find_in_urlencoded_list_opt field =
  let open Pool_message in
  find_in_urlencoded_base_opt Field.(array_key field) %> CCOption.value ~default:[]
;;

let find_in_urlencoded_opt field =
  let open Pool_message in
  find_in_urlencoded_base_opt Field.(show field) %> flip CCOption.bind CCList.head_opt
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
  | [ hd ] when CCString.equal "undefined" (hd |> CCString.lowercase_ascii) -> []
  | lst -> lst
;;

(* TODO[timhub]: hide information, at least on public site *)
let validate_email_existance pool email =
  let open Utils.Lwt_result.Infix in
  Pool_user.find_by_email_opt pool email
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
        | Some values -> values |> intersection_to_bool_string |> CCOption.return)
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

let externalize_path_with_params params path =
  url_with_field_params params path |> Sihl.Web.externalize_path
;;

let add_line_breaks = Utils.Html.handle_line_breaks Tyxml.Html.span

let invalid_session_redirect
      ?(login_path = fun req -> "/login" |> intended_of_request req)
      req
      url_parameters
  =
  redirect_to_with_actions
    (url_with_field_params url_parameters (login_path req))
    [ Message.set ~error:[ Pool_message.Error.SessionInvalid ] ]
;;

let find_id encode field req =
  Sihl.Web.Router.param req @@ Pool_message.Field.show field |> encode
;;

let find_id_save encode field req =
  let open Pool_message in
  try find_id encode field req with
  | _ -> Error Error.(NotFound field)
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
  if CCString.length m > n then CCString.sub m 0 n |> Format.asprintf "%s..." else m
;;

module Htmx = struct
  let hx_request_header = "Hx-Request"

  let is_hx_request req =
    let headers = Rock.Request.(req.headers) in
    match Httpaf.Headers.get headers hx_request_header with
    | Some "true" -> true
    | _ -> false
  ;;

  let headers = Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]

  let htmx_redirect
        ?(skip_externalize = false)
        ?(query_parameters = [])
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
         ("HX-Redirect", url_with_field_params query_parameters path |> externalize_path)
    |> CCList.fold_left ( % ) id actions
    |> Lwt.return
  ;;

  (* TODO: REMOVE *)
  let html_to_plain_text_response ?(status = 200) html =
    html
    |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
    |> Sihl.Web.Response.of_plain_text ~status:(status |> Opium.Status.of_code) ~headers
  ;;

  let multi_html_to_plain_text_response ?(status = 200) html_els =
    html_els
    |> CCList.fold_left
         (fun acc cur -> Format.asprintf "%s\n%a" acc (Tyxml.Html.pp_elt ()) cur)
         ""
    |> Sihl.Web.Response.of_plain_text ~status:(status |> Opium.Status.of_code) ~headers
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
    Logs.err ~src (fun m -> m ~tags "Context not found: %s" (Pool_message.Error.show err));
    htmx_redirect "/error" ()
  ;;

  let handle_error_message ?(src = src) ?(error_as_notification = false) req result =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    match context with
    | Ok ({ Pool_context.language; _ } as context) ->
      let%lwt res = result context in
      res
      |> CCResult.get_lazy (fun error_msg ->
        let err = error_msg |> Pool_common.Utils.with_log_error in
        let html =
          if error_as_notification
          then error_notification language err
          else inline_error language err
        in
        html_to_plain_text_response html)
      |> Lwt.return
    | Error err -> context_error ~src ~tags err
  ;;

  let extract_happy_path ?(src = src) req result =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    match context with
    | Ok ({ Pool_context.query_parameters; _ } as context) ->
      let%lwt res = result context in
      res
      |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _) -> err)
      |> CCResult.map Lwt.return
      |> CCResult.get_lazy (fun (error_msg, error_path) ->
        htmx_redirect
          error_path
          ~query_parameters
          ~actions:[ Message.set ~error:[ error_msg ] ]
          ())
    | Error err -> context_error ~src ~tags err
  ;;
end

(* TODO: This module should probably be made obsolete, the API response module can be used instead *)
module Json = struct
  let yojson_response ?status json =
    let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ] in
    json |> Sihl.Web.Response.of_json ?status ~headers |> Lwt.return
  ;;

  let handle_yojson_response ?(src = src) req result =
    let context = Pool_context.find req in
    let tags = Pool_context.Logger.Tags.req req in
    let return_error language err =
      yojson_response
        ~status:(Opium.Status.of_code 400)
        (`Assoc [ "message", `String Pool_common.(Utils.error_to_string language err) ])
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
