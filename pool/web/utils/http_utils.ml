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
