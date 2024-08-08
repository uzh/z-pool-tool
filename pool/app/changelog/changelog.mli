module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module T : functor (R : RecordSig) -> sig
  type t =
    { changes : Yojson.Safe.t
    ; model : Pool_message.Field.t
    ; user_id : Pool_common.Id.t
    }

  type record = R.t

  val model : Pool_message.Field.t
  val create : user_id:Pool_common.Id.t -> R.t -> R.t -> t
end

module type TSig = sig
  type t
  type record

  val model : Pool_message.Field.t
  val create : user_id:Pool_common.Id.t -> record -> record -> t
end
