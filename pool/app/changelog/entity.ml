module type RecordSig = sig
  type t

  val yojson_of_t : t -> Yojson.Safe.t
end

module T (R : RecordSig) = struct
  type t = Yojson.Safe.t
  type record = R.t

  let to_json t = t

  let create (before : R.t) (after : R.t) : t =
    let rec compare (json_before : Yojson.Safe.t) (json_after : Yojson.Safe.t)
      : t option
      =
      match json_before, json_after with
      | `Assoc l1, `Assoc l2 ->
        l1
        |> List.filter_map (fun (key, value_before) ->
          (* TODO: assoc opt *)
          let value_after = List.assoc key l2 in
          compare value_before value_after
          |> CCOption.map (fun value -> key, value))
        |> (function
         | [] -> None
         | assoc -> Some (`Assoc assoc))
      | _ ->
        (match Yojson.Safe.equal json_before json_after with
         | true -> None
         | false -> Some (`Tuple [ json_before; json_after ]))
    in
    compare (R.yojson_of_t before) (R.yojson_of_t after)
    |> CCOption.value ~default:(`Assoc [])
  ;;
end

module type TSig = sig
  type t
  type record

  val to_json : t -> Yojson.Safe.t
  val create : record -> record -> t
end
