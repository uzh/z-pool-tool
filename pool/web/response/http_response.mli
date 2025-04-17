type url_encoded = (string * string list) list

type http_error =
  | AccessDenied
  | BadRequest of
      (Rock.Request.t -> Rock.Response.t Lwt.t)
      * url_encoded option
      * Pool_message.Error.t
  | NotFound of Pool_message.Error.t

val access_denied : http_error

val bad_request
  :  ?urlencoded:url_encoded
  -> (Rock.Request.t -> Rock.Response.t Lwt.t)
  -> Pool_message.Error.t
  -> http_error

val not_found : Pool_message.Error.t -> http_error

val not_found_on_error
  :  ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

val bad_request_on_error
  :  ?urlencoded:url_encoded
  -> (Rock.Request.t -> Rock.Response.t Lwt.t)
  -> ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

val handle
  :  ?src:Logs.src
  -> ?enable_cache:bool
  -> Rock.Request.t
  -> (Pool_context.t -> (Rock.Response.t, http_error) Lwt_result.t)
  -> Rock.Response.t Lwt.t
