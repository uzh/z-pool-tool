val make
  :  string
  -> maintenance_handler:(unit -> Rock.Response.t Lwt.t)
  -> connection_issue_handler:(unit -> Rock.Response.t Lwt.t)
  -> error_handler:(Pool_message.Error.t -> Rock.Response.t Lwt.t)
  -> unit
  -> Rock.Middleware.t

val validate : unit -> Rock.Middleware.t
