module PartialUpdate = Helpers_partial_update

module type AccessSig = sig
  val index : Rock.Middleware.t
  val create : Rock.Middleware.t
  val read : Rock.Middleware.t
  val update : Rock.Middleware.t
  val delete : Rock.Middleware.t
end
