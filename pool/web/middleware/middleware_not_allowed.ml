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
    let query_language = Http_utils.find_query_lang req in
    Http_utils.(
      Htmx.htmx_redirect
        "/login"
        ~status:`Forbidden
        ?query_language
        ~actions:[ Message.set ~error:[ Pool_common.Message.SessionInvalid ] ])
      ()
;;
