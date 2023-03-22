module File = Http_utils_file
module Filter = Http_utils_filter
module Message = Http_utils_message
module StringMap = CCMap.Make (CCString)

let src = Logs.Src.create "http_utils"

type json_response =
  { message : string
  ; success : bool
  }
[@@deriving yojson]

let find_intended_opt req =
  let open Uri in
  let key = Pool_common.Message.Field.(location |> show) in
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
  let open CCFun in
  let open Uri in
  let key = Pool_common.Message.Field.(location |> show) in
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
  | Some intended -> CCFun.flip intended_to_url intended
  | None -> CCFun.flip intended_to_url target
;;

let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_tenant.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let get_field_router_param req field =
  Sihl.Web.Router.param req Pool_common.Message.Field.(field |> show)
;;

let find_field_router_param_opt req field =
  try Some (get_field_router_param req field) with
  | _ -> None
;;

let find_query_lang req =
  let open CCFun in
  let open CCOption.Infix in
  Sihl.Web.Request.query Pool_common.Message.Field.(Language |> show) req
  >>= CCString.uppercase_ascii
      %> Pool_common.Language.create
      %> CCOption.of_result
;;

let path_with_language lang path =
  lang
  |> CCOption.map (fun lang ->
       let open Pool_common in
       Message.add_field_query_params
         path
         [ ( Message.Field.Language
           , lang |> Language.show |> CCString.lowercase_ascii )
         ])
  |> CCOption.value ~default:path
;;

let redirect_to_with_actions path actions =
  path
  |> Sihl.Web.externalize_path
  |> Sihl.Web.Response.redirect_to
  |> CCList.fold_left CCFun.( % ) CCFun.id actions
  |> Lwt.return
;;

let redirect_to path = redirect_to_with_actions path []

let extract_happy_path_generic req result msgf =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_language; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~tags (fun (err, _) -> err)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_msg, error_path) ->
         redirect_to_with_actions
           (path_with_language query_language error_path)
           [ msgf error_msg ])
  | Error err ->
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Message.Message.show_error err));
    redirect_to "/error"
;;

let extract_happy_path req result =
  extract_happy_path_generic req result (fun err ->
    Logs.warn ~src (fun m ->
      m
        ~tags:(Pool_context.Logger.Tags.req req)
        "A user experienced an error: %s"
        (Message.Message.show_error err));
    Message.set ~warning:[] ~success:[] ~info:[] ~error:[ err ])
;;

let extract_happy_path_with_actions req result =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_language; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~tags (fun (err, _, _) -> err)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_key, error_path, error_actions) ->
         redirect_to_with_actions
           (path_with_language query_language error_path)
           (CCList.append
              [ Message.set
                  ~warning:[]
                  ~success:[]
                  ~info:[]
                  ~error:[ error_key ]
              ]
              error_actions))
  | Error err ->
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Message.Message.show_error err));
    redirect_to "/error"
;;

let htmx_redirect path ?query_language ?(actions = []) () =
  Sihl.Web.Response.of_plain_text ""
  |> Sihl.Web.Response.add_header
       ( "HX-Redirect"
       , path_with_language query_language path |> Sihl.Web.externalize_path )
  |> CCList.fold_left CCFun.( % ) CCFun.id actions
  |> Lwt.return
;;

let extract_happy_path_htmx req result =
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok ({ Pool_context.query_language; _ } as context) ->
    let%lwt res = result context in
    res
    |> Pool_common.Utils.with_log_result_error ~tags (fun (err, _) -> err)
    |> CCResult.map Lwt.return
    |> CCResult.get_lazy (fun (error_msg, error_path) ->
         htmx_redirect
           error_path
           ?query_language
           ~actions:[ Message.set ~error:[ error_msg ] ]
           ())
  | Error err ->
    Logs.err ~src (fun m ->
      m ~tags "%s" Pool_common.(Utils.error_to_string Language.En err));
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Message.Message.show_error err));
    htmx_redirect "/error" ()
;;

(* Read urlencoded values in any order *)
let urlencoded_to_params_opt urlencoded keys =
  keys
  |> CCList.map
     @@ fun key -> key, CCList.assoc_opt ~eq:CCString.equal key urlencoded
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

(* This is required as HTMX sends "undefined" if all checkboxes are unchecked *)
let htmx_urlencoded_list key req =
  let%lwt lst = Sihl.Web.Request.urlencoded_list key req in
  Lwt.return
  @@
  match lst with
  | [ hd ] ->
    if CCString.equal "undefined" (hd |> CCString.lowercase_ascii)
    then []
    else lst
  | _ -> lst
;;

(* TODO[timhub]: hide information, at least on public site *)
let validate_email_existance pool email =
  let open Utils.Lwt_result.Infix in
  Service.User.find_by_email_opt ~ctx:(Pool_tenant.to_ctx pool) email
  ||> function
  | None -> Ok ()
  | Some _ -> Error Pool_common.Message.EmailAlreadyInUse
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
  let ingnore_fields =
    CCList.map Pool_common.Message.Field.show ingnore_fields
  in
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

let html_to_plain_text_response html =
  let headers =
    Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]
  in
  html
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  |> Sihl.Web.Response.of_plain_text ~headers
;;

let yojson_response ?status json =
  let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ] in
  json |> Sihl.Web.Response.of_json ?status ~headers
;;

let multi_html_to_plain_text_response ?(status = 200) html_els =
  let headers =
    Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]
  in
  html_els
  |> CCList.fold_left
       (fun acc cur -> Format.asprintf "%s\n%a" acc (Tyxml.Html.pp_elt ()) cur)
       ""
  |> Sihl.Web.Response.of_plain_text
       ~status:(status |> Opium.Status.of_code)
       ~headers
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
    [ Message.set ~error:[ Pool_common.Message.SessionInvalid ] ]
;;

let find_id encode field req =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field |> encode
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
