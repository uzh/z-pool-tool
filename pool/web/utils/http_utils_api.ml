let api_request_header = "Api-Request"

let is_api_request req =
  let headers = Rock.Request.(req.headers) in
  match Httpaf.Headers.get headers api_request_header with
  | Some "true" -> true
  | _ -> false
;;

let find_id validate_and_encode field req =
  Sihl.Web.Router.param req @@ Pool_message.Field.show field |> validate_and_encode
;;
