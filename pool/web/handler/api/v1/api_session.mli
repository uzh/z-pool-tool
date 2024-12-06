val show : Rock.Request.t -> Rock.Response.t Lwt.t

module Access : sig
  val read : Rock.Middleware.t
end
