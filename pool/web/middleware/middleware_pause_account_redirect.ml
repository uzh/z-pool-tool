let middleware () =
  let filter handler req =
    let uri = Uri.of_string req.Opium.Request.target in
    match Uri.path uri with
    | "/user/pause-account" ->
      Http_utils.retain_url_params req "/unsubscribe"
      |> Uri.to_string
      |> Http_utils.redirect_to
    | _ -> handler req
  in
  Rock.Middleware.create ~name:"pause_account.redirect" ~filter
;;
