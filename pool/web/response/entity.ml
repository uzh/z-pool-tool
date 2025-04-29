open Pool_message

let map_error = Utils.Lwt_result.map_error

type url_encoded = (string * string list) list

type http_error =
  | AccessDenied
  | BadRequest of (Rock.Request.t -> Rock.Response.t Lwt.t) * url_encoded option * Error.t
  | NotFound of Error.t

let error_message = function
  | AccessDenied -> Error.AccessDenied
  | BadRequest (_, _, err) -> err
  | NotFound err -> err
;;

let access_denied = AccessDenied
let bad_request ?urlencoded f err = BadRequest (f, urlencoded, err)

let bad_request_on_error ?urlencoded fallback req =
  map_error (bad_request ?urlencoded fallback) req
;;

let not_found err = NotFound err
let not_found_on_error req = map_error not_found req
