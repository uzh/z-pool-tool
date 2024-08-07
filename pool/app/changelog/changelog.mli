module type RecordSig = sig
  type t

  val yojson_of_t : t -> Yojson.Safe.t
end

module T : functor (R : RecordSig) -> sig
  type t
  type record = R.t

  val to_json : t -> Yojson.Safe.t
  val create : R.t -> R.t -> t
end

module type TSig = sig
  type t
  type record

  val to_json : t -> Yojson.Safe.t
  val create : record -> record -> t
end
