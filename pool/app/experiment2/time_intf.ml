[@@@warning "-32"]

module type Intf = sig
  type t [@@deriving eq, show]

  val make
    :  Ptime.Span.t
    -> (t, [> `Time_error of [> `Negative_amount ] ]) result

  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val schema : t Caqti_type.t
end
