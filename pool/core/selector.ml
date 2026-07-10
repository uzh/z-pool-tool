(* The dependency-free base of a selector type: an enum-derived variant that
   can be listed exhaustively and read back from its yojson [@name]
   representation. Error handling and conformist schemas build on top of this
   in [Pool_model.Base.SelectorType]. *)

module type EnumSig = sig
  type t

  val min : int
  val max : int
  val of_enum : int -> t option
  val t_of_yojson : Yojson.Safe.t -> t
end

module Make (Core : EnumSig) = struct
  let to_yojson_string m = Format.asprintf "[\"%s\"]" m
  let read m = m |> to_yojson_string |> Yojson.Safe.from_string |> Core.t_of_yojson

  let all : Core.t list =
    CCList.range Core.min Core.max
    |> CCList.map Core.of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Selector: could not create list of all keys!"
  ;;
end
