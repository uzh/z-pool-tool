include Entity
include Event

module T (R : RecordSig) = struct
  type record = R.t

  let model = R.model

  let column_created_at =
    (Pool_message.Field.CreatedAt, "pool_change_log.created_at")
    |> Query.Column.create
  ;;

  let searchable_by = []
  let sortable_by = []
  let filterable_by = None

  let default_sort =
    let open Query in
    Sort.{ column = column_created_at; order = SortOrder.Descending }
  ;;

  let default_query = Query.create ~sort:default_sort ()

  let make_changes before after : Changes.t option =
    let open Changes in
    let rec compare json_before json_after =
      let eq = CCString.equal in
      let make_changes before after =
        match Yojson.Safe.equal before after with
        | true -> None
        | false -> Some (Change (before, after))
      in
      let list_to_assoc (json_list : Yojson.Safe.t list) =
        let rec check acc = function
          | [] -> Some (`Assoc acc)
          | `List [ key; value ] :: tl ->
            check (acc @ [ Yojson.Safe.to_string key, value ]) tl
          | _ -> None
        in
        check [] json_list
      in
      match (json_before : Yojson.Safe.t), (json_after : Yojson.Safe.t) with
      | `Assoc l1, `Assoc l2 ->
        let keys =
          let get = CCList.map fst in
          get l1 @ get l2 |> CCList.uniq ~eq
        in
        keys
        |> CCList.filter_map (fun key ->
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
        else
          Some (Change (json_before, json_after))
          (* Catching equal assoc lists *)
      | `List l1, `List l2 ->
        (match list_to_assoc l1, list_to_assoc l2 with
         | Some assoc_before, Some assoc_after ->
           compare assoc_before assoc_after
         | None, None ->
           let open CCList.Infix in
           let open CCOption.Infix in
           let after = Hashtbl.create (CCList.length l2) in
           let () = l2 |> CCList.iteri (fun i x -> Hashtbl.add after i x) in
           let changes_before =
             l1
             |> CCList.foldi
                  (fun acc i before_json ->
                    Hashtbl.find_opt after i
                    |> CCOption.value ~default:`Null
                    |> compare before_json
                    >|= CCPair.make (CCInt.to_string i)
                    |> fun change ->
                    let () = Hashtbl.remove after i in
                    match change with
                    | None -> acc
                    | Some change -> acc @ [ change ])
                  []
           in
           let changes_after =
             after
             |> Hashtbl.to_seq
             |> CCList.of_seq
             |> CCList.filter_map (fun (i, json) ->
               make_changes `Null json >|= CCPair.make (CCInt.to_string i))
           in
           (match changes_before @ changes_after with
            | [] -> None
            | changes -> Some (Assoc changes))
         | _, _ -> make_changes json_before json_after)
      | _ -> make_changes json_before json_after
    in
    compare (R.yojson_of_t before) (R.yojson_of_t after)
  ;;

  let make_write
    ?(id = Id.create ())
    ?user_uuid
    ~entity_uuid
    (before : R.t)
    (after : R.t)
    =
    (*** the entity_uuid could also be part of the Reord module, as a function.
      This would probably require us to create a Record module for each model,
      but provide more flexibility *)
    make_changes before after
    |> CCOption.map (fun changes ->
      Write.
        { id
        ; changes
        ; model
        ; entity_uuid
        ; user_uuid
        ; created_at = Pool_common.CreatedAt.create_now ()
        })
  ;;

  let create ?(id = Id.create ()) ?user_uuid ~entity_uuid ~before ~after () =
    make_write ~id ~entity_uuid ?user_uuid before after
  ;;

  let insert pool ?user_uuid ~entity_uuid ~before ~after () =
    create ~entity_uuid ?user_uuid ~before ~after ()
    |> function
    | Some changelog -> Repo.insert pool changelog
    | None -> Lwt.return_unit
  ;;

  let all_by_entity = Repo.find_by_model model
end
