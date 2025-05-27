exception Exception of string

module Make : functor (_ : Pools_sig.ConfigSig) -> Pools_sig.Sig
