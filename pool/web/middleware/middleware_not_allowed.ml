let htmx_request_key = "HX-Request"

let handle req =
  let open Opium in
  let is_htmx =
    Headers.get req.Request.headers htmx_request_key
    |> CCOption.map (CCString.equal "true")
    |> CCOption.value ~default:false
  in
  match is_htmx with
  | false -> Response.of_plain_text ~status:`Forbidden "" |> Lwt.return
  | true ->
    Http_utils.(
      Http_response.Htmx.redirect
        (retain_url_params req "/login" |> Uri.to_string)
        ~status:`Forbidden
        ~actions:[ Message.set ~error:[ Pool_message.Error.SessionInvalid ] ])
;;
