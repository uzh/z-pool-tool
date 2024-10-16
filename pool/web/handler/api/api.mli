module Experiment : sig
  val show : Rock.Request.t -> Rock.Response.t Lwt.t

  module Access : sig
    val read : Rock.Middleware.t
  end
end

module OrganisationalUnit : sig
  val index : Rock.Request.t -> Rock.Response.t Lwt.t
end

val not_found : Rock.Request.t -> Rock.Response.t Lwt.t
