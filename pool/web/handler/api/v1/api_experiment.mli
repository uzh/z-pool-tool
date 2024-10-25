val index : Rock.Request.t -> Rock.Response.t Lwt.t
val show : Rock.Request.t -> Rock.Response.t Lwt.t

module Access : sig
  val index : Rock.Middleware.t
  val read : Rock.Middleware.t
end
