module Message = Http_utils_message

let redirect_to_with_actions path actions =
  path
  |> Sihl.Web.externalize_path
  |> Sihl.Web.Response.redirect_to
  |> CCList.fold_left CCFun.( % ) CCFun.id actions
  |> Lwt.return
;;

let extract_happy_path_generic result msgf =
  result
  |> Result.map Lwt.return
  |> CCResult.get_lazy (fun (error_msg, error_path) ->
         redirect_to_with_actions error_path [ msgf error_msg ])
;;

let extract_happy_path result =
  extract_happy_path_generic result (fun err ->
      Message.set ~warning:[] ~success:[] ~info:[] ~error:[ err ])
;;

let extract_happy_path_with_actions result =
  result
  |> Result.map Lwt.return
  |> CCResult.get_lazy (fun (error_msg, error_path, error_actions) ->
         redirect_to_with_actions
           error_path
           (List.append
              [ Message.set
                  ~warning:[]
                  ~success:[]
                  ~info:[]
                  ~error:[ error_msg ]
              ]
              error_actions))
;;

(* Read urlencoded values in any order *)
let urlencoded_to_params_opt urlencoded keys =
  keys |> CCList.map @@ fun key -> key, List.assoc_opt key urlencoded
;;

let urlencoded_to_params urlencoded keys =
  keys
  |> (CCList.map
     @@ fun key ->
     Option.bind (List.assoc_opt key urlencoded) CCList.head_opt
     |> Option.map @@ CCPair.make key)
  |> CCList.all_some
;;

let request_to_params req keys () =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  urlencoded_to_params urlencoded keys
  |> CCOpt.to_result "Please provide necessary fields"
  |> Lwt_result.lift
;;

let err_with_action ?message error_path action =
  Lwt_result.map_err (fun msg ->
      match message with
      | None -> msg, error_path, action
      | Some msg -> msg, error_path, action)
;;

let user_email_exists email =
  let open Lwt.Syntax in
  let* user = Service.User.find_by_email_opt email in
  match user with
  | None -> Lwt_result.return ()
  | Some _ -> Lwt_result.fail "Email address is already in use."
;;
