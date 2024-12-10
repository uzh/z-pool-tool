module Reason : sig
  type t = MergedDuplicate
end

val insert_request : (string * Reason.t, unit, [ `Zero ]) Caqti_request.t
