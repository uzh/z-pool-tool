module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module T (R : RecordSig) = struct
  type t =
    { changes : Yojson.Safe.t
    ; model : Pool_message.Field.t
    ; user_id : Pool_common.Id.t
    }

  type record = R.t

  let model = R.model

  let create ~user_id (before : R.t) (after : R.t) =
    let rec compare json_before json_after =
      let eq = CCString.equal in
      match json_before, json_after with
      | `Assoc l1, `Assoc l2 ->
        let keys =
          let get = CCList.map fst in
          get l1 @ get l2 |> CCList.uniq ~eq
        in
        keys
        |> List.filter_map (fun key ->
          let assoc list =
            CCList.assoc_opt ~eq key list |> CCOption.value ~default:`Null
          in
          let value_before = assoc l1 in
          let value_after = assoc l2 in
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
    let changes =
      compare (R.yojson_of_t before) (R.yojson_of_t after)
      |> CCOption.value ~default:`Null
    in
    { changes; model; user_id }
  ;;
end

module type TSig = sig
  type t
  type record

  val model : Pool_message.Field.t
  val create : user_id:Pool_common.Id.t -> record -> record -> t
end
