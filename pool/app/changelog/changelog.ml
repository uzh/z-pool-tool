include Entity

module T (R : RecordSig) = struct
  type record = R.t

  let model = R.model

  let make_changes before after : Changes.t option =
    let open Changes in
    let rec compare json_before json_after =
      let eq = CCString.equal in
      match (json_before : Yojson.Safe.t), (json_after : Yojson.Safe.t) with
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
         | assoc -> Some (Assoc assoc))
      (* Catching equal variants *)
      | `Tuple [ `String key1; val1 ], `Tuple [ `String key2; val2 ]
      | `List [ `String key1; val1 ], `List [ `String key2; val2 ] ->
        if eq key1 key2
        then compare val1 val2
        else Some (Change (json_before, json_after))
      | _ ->
        (match Yojson.Safe.equal json_before json_after with
         | true -> None
         | false ->
           (* This is triggered on the highest level, nested changes are not
              considered *)
           Some (Change (json_before, json_after)))
    in
    compare (R.yojson_of_t before) (R.yojson_of_t after)
  ;;

  let make
    ?(id = Id.create ())
    ~entity_uuid
    ~user_uuid
    (before : R.t)
    (after : R.t)
    =
    (*** the entity_uuid could also be part of the Reord module, as a function.
      This would probably require us to create a Record module for each model,
      but provide more flexibility *)
    make_changes before after
    |> CCOption.map (fun changes ->
      { id
      ; changes
      ; model
      ; entity_uuid
      ; user_uuid
      ; created_at = Pool_common.CreatedAt.create_now ()
      })
  ;;

  let create pool ?(id = Id.create ()) ~entity_uuid ~user_uuid ~before ~after ()
    =
    make ~id ~entity_uuid ~user_uuid before after
    |> CCOption.map_or ~default:Lwt.return_unit (Repo.insert pool)
  ;;

  let all_by_entity ?query pool entity_uuid =
    Repo.find_by_model ?query pool model entity_uuid
  ;;
end
