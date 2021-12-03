module Message = Http_utils_message
module File = Http_utils_file
module StringMap = CCMap.Make (CCString)

let redirect_to_with_actions path actions =
  path
  |> Sihl.Web.externalize_path
  |> Sihl.Web.Response.redirect_to
  |> CCList.fold_left CCFun.( % ) CCFun.id actions
  |> Lwt.return
;;

let redirect_to path = redirect_to_with_actions path []

let extract_happy_path_generic result msgf =
  result
  |> Pool_common.Utils.with_log_result_error (fun (err, _) -> err)
  |> CCResult.map Lwt.return
  |> CCResult.get_lazy (fun (error_msg, error_path) ->
         redirect_to_with_actions error_path [ msgf error_msg ])
;;

let extract_happy_path result =
  extract_happy_path_generic result (fun err ->
      Message.set ~warning:[] ~success:[] ~info:[] ~error:[ err ])
;;

let extract_happy_path_with_actions result =
  result
  |> Pool_common.Utils.with_log_result_error (fun (err, _, _) -> err)
  |> CCResult.map Lwt.return
  |> CCResult.get_lazy (fun (error_key, error_path, error_actions) ->
         redirect_to_with_actions
           error_path
           (CCList.append
              [ Message.set
                  ~warning:[]
                  ~success:[]
                  ~info:[]
                  ~error:[ error_key ]
              ]
              error_actions))
;;

(* Read urlencoded values in any order *)
let urlencoded_to_params_opt urlencoded keys =
  keys
  |> CCList.map @@ fun key -> key, CCList.assoc_opt ~eq:( = ) key urlencoded
;;

let urlencoded_to_params urlencoded keys =
  keys
  |> (CCList.map
     @@ fun key ->
     CCOption.bind (List.assoc_opt key urlencoded) CCList.head_opt
     |> CCOption.map @@ CCPair.make key)
  |> CCList.all_some
;;

let request_to_params req keys () =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  urlencoded_to_params urlencoded keys
  |> CCOption.to_result Pool_common.Message.RequestRequiredFields
  |> Lwt_result.lift
;;

let urlencoded_to_flash urlencoded =
  Sihl.Web.Flash.set (urlencoded |> CCList.map (fun (m, k) -> m, CCList.hd k))
;;

let validate_email_existance pool email =
  let open Lwt.Infix in
  Service.User.find_by_email_opt ~ctx:(Pool_common.Utils.pool_to_ctx pool) email
  >|= function
  | None -> Ok ()
  | Some _ -> Error Pool_common.Message.EmailAlreadyInUse
;;

let handled_true_values = [ "on"; "checked"; "true" ]

let handle_boolean_values update urlencoded values =
  let urlencoded = urlencoded |> CCList.to_seq |> StringMap.of_seq in
  CCList.fold_left update urlencoded values |> StringMap.to_seq |> CCList.of_seq
;;

let format_request_boolean_values values urlencoded =
  let update m k =
    StringMap.update
      k
      (function
        | None -> Some [ "false" ]
        | Some values ->
          CCList.flat_map
            (fun v -> CCList.map (CCString.equal v) handled_true_values)
            values
          |> CCList.exists CCFun.id
          |> string_of_bool
          |> CCList.pure
          |> CCOption.some)
      m
  in
  handle_boolean_values update urlencoded values
;;

let format_htmx_request_boolean_values values urlencoded =
  let update m k =
    StringMap.update
      k
      (fun values ->
        values
        |> CCOption.map (fun v ->
               CCList.flat_map
                 (fun v -> CCList.map (CCString.equal v) handled_true_values)
                 v
               |> CCList.exists CCFun.id
               |> string_of_bool
               |> CCList.pure))
      m
  in
  handle_boolean_values update urlencoded values
;;

let placeholder_from_name = CCString.replace ~which:`All ~sub:"_" ~by:" "
let find_csrf req = Sihl.Web.Csrf.find_exn req

let is_req_from_root_host req =
  req
  |> Sihl.Web.Request.header "host"
  |> CCOption.map2 CCString.equal_caseless Utils.Url.public_host
  |> CCOption.value ~default:false
;;

let changeset_of_request req =
  Opium.Request.cookie "_changeset" req
  |> CCOption.map Pool_common.ChangeSet.of_string
  |> CCOption.value ~default:Pool_common.ChangeSet.empty
;;

let remove_csrf_from_urlencoded
    : (string * string list) list -> (string * string list) list
  =
  CCList.filter (fun (key, _) -> not (CCString.equal key "_csrf"))
;;

let html_to_plain_text_response html =
  let headers =
    Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]
  in
  html
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  |> Sihl.Web.Response.of_plain_text ~headers
;;
