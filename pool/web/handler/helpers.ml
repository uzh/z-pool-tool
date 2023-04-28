module Login = Helpers_login
module PartialUpdate = Helpers_partial_update

module Access : sig
  val index : Rock.Middleware.t
  val create : Rock.Middleware.t
  val read : Rock.Middleware.t
  val update : Rock.Middleware.t
  val delete : Rock.Middleware.t
end = struct
  module Guardian = Middleware.Guardian

  let index = Guardian.denied
  let create = Guardian.denied
  let read = Guardian.denied
  let update = Guardian.denied
  let delete = Guardian.denied
end
