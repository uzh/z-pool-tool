type http_error =
  | AccessDenied
  | BadRequest of (Rock.Request.t -> Rock.Response.t Lwt.t) * Pool_message.Error.t
  | NotFound of Pool_message.Error.t

val accessdenied : http_error

val badrequest
  :  (Rock.Request.t -> Rock.Response.t Lwt.t)
  -> Pool_message.Error.t
  -> http_error

val notfound : Pool_message.Error.t -> http_error

val handle
  :  ?src:Logs.src
  -> ?enable_cache:bool
  -> Rock.Request.t
  -> (Pool_context.t -> (Rock.Response.t, http_error) result Lwt.t)
  -> Rock.Response.t Lwt.t
