module OrganisationalUnit : sig
  val index : Rock.Request.t -> Rock.Response.t Lwt.t
end

val not_found : Rock.Request.t -> Rock.Response.t Lwt.t
