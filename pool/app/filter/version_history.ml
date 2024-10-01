module Record = struct
  open Entity

  type t = Entity.t

  (* This function provides a more human readable json representation than the
     yojson_ppx *)
  let yojson_of_t { id; query; title; created_at; updated_at } =
    let open Utils.Ptime in
    let open Pool_common in
    let predicate_key (key : Key.t) =
      let open Key in
      match key with
      | CustomField id -> Custom_field.Id.value id
      | Hardcoded key -> show_hardcoded key
    in
    let yojson_of_predicate ({ Predicate.operator; value; _ } : Predicate.t) =
      let yojson_of_value m =
        match m with
        | NoValue -> `Null
        | Single single -> single |> yojson_of_single_val
        | Lst values -> `List (CCList.map yojson_of_single_val values)
      in
      let operator = Operator.yojson_of_t operator in
      let value = yojson_of_value value in
      `Assoc Helper.[ operator_string, operator; value_string, value ]
    in
    let rec yojson_of_query f : Yojson.Safe.t =
      (match f with
       | And queries -> show_query f, `List (CCList.map yojson_of_query queries)
       | Or queries -> show_query f, `List (CCList.map yojson_of_query queries)
       | Not f -> show_query f, f |> yojson_of_query
       | Pred p ->
         let open Predicate in
         predicate_key p.key, yojson_of_predicate p
       | Template id -> show_query f, `String (Id.value id))
      |> fun (key, pred) -> `Assoc [ key, pred ]
    in
    `Assoc
      [ "id", `String (Pool_common.Id.value id)
      ; "query", yojson_of_query query
      ; "title", `String (title |> Option.value ~default:"")
      ; "created_at", CreatedAt.value created_at |> yojson_of_ptime
      ; "updated_at", UpdatedAt.value updated_at |> yojson_of_ptime
      ]
  ;;

  let model = Pool_message.Field.Filter
end

include Changelog.T (Record)

let changelog_to_human pool language ({ Changelog.changes; _ } as changelog) =
  let open Changelog.Changes in
  let id_of_string = CCFun.(Custom_field.Id.validate %> CCOption.of_result) in
  let yojson_id = function
    | `String id -> id_of_string id
    | _ -> None
  in
  let rec collect_uuids acc = function
    | Assoc lst ->
      List.fold_left
        (fun acc (key, v) ->
          let uuids =
            id_of_string key
            |> function
            | None -> acc
            | Some uuid -> uuid :: acc
          in
          collect_uuids uuids v)
        acc
        lst
    | Change (before, after) ->
      CCList.filter_map yojson_id [ before; after ] @ acc
  in
  let get_or = CCOption.get_or in
  let uuids = collect_uuids [] changes in
  let%lwt names = Custom_field.find_names pool uuids in
  let tbl = Hashtbl.create (CCList.length names) in
  let () =
    CCList.iter
      (fun (id, name) ->
        let name =
          let open Custom_field.Name in
          find_opt language name
          |> CCOption.value ~default:(get_hd name)
          |> value_name
        in
        Hashtbl.add tbl (Pool_common.Id.value id) name)
      names
  in
  let rec replace_names = function
    | Assoc lst ->
      Assoc
        (CCList.map
           (fun (key, changes) ->
             let key = Hashtbl.find_opt tbl key |> get_or ~default:key in
             let changes = replace_names changes in
             key, changes)
           lst)
    | Change (before, after) ->
      let replace = function
        | `String str ->
          `String (Hashtbl.find_opt tbl str |> get_or ~default:str)
        | change -> change
      in
      Change (replace before, replace after)
  in
  let changes = replace_names changes in
  Lwt.return Changelog.{ changelog with changes }
;;
