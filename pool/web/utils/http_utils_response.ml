open CCFun
module Message = Http_utils_message

let src = Logs.Src.create "http_utils.response"
let url_with_field_params path params = Pool_message.add_field_query_params params path

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

let extract_happy_path_generic ?(src = src) ?enable_cache req result msgf =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_parameters; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _) -> err)
    |> CCResult.map (set_no_cache_headers ?enable_cache)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_msg, error_path) ->
      redirect_to_with_actions
        (url_with_field_params query_parameters error_path)
        [ msgf error_msg ])
  | Error err ->
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Pool_message.Error.show err));
    redirect_to "/error"
;;

let extract_happy_path ?(src = src) ?enable_cache req result =
  extract_happy_path_generic ~src ?enable_cache req result (fun err ->
    let err =
      Pool_common.Utils.with_log_error ~src ~tags:(Pool_context.Logger.Tags.req req) err
    in
    Message.set ~warning:[] ~success:[] ~info:[] ~error:[ err ])
;;

let extract_happy_path_with_actions ?(src = src) ?enable_cache req result =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_parameters; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~src ~tags (fun (err, _, _) -> err)
    |> CCResult.map (set_no_cache_headers ?enable_cache)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_key, error_path, error_actions) ->
      redirect_to_with_actions
        (url_with_field_params query_parameters error_path)
        (CCList.append
           [ Message.set ~warning:[] ~success:[] ~info:[] ~error:[ error_key ] ]
           error_actions))
  | Error err ->
    Logs.err ~src (fun m -> m ~tags "Context not found: %s" (Pool_message.Error.show err));
    redirect_to "/error"
;;

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

let invalid_session_redirect
      ?(login_path = fun req -> "/login" |> intended_of_request req)
      req
      url_parameters
  =
  redirect_to_with_actions
    (url_with_field_params url_parameters (login_path req))
    [ Message.set ~error:[ Pool_message.Error.SessionInvalid ] ]
;;
