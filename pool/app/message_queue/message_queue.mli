module type Sig = Service.Sig

module type Config = sig
  val host : string
  val credentials : string * string
end

module Make : functor (_ : Config) -> Sig
