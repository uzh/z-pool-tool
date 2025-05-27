let print = Entity.print

type t =
  | And of t list [@printer print "and"]
  | Or of t list [@printer print "or"]
  | Not of t [@printer print "not"]
  | Pred of Entity.Predicate.human [@printer print "pred"]
  | Template of Pool_common.Id.t option [@printer print "template"]
[@@deriving show { with_path = false }]

let init ?key ?operator ?value () : t =
  Pred (Entity.Predicate.create_human ?key ?operator ?value ())
;;

let value_of_yojson_opt (yojson : Yojson.Safe.t) =
  let open CCOption in
  let open CCFun in
  match yojson with
  | `Assoc [ (key, value) ] ->
    (match key, value with
     | "list", `List values ->
       values
       |> CCList.filter_map (Entity.single_value_of_yojson %> of_result)
       |> Entity.lst
       |> CCOption.pure
     | _ -> Entity.single_value_of_yojson yojson |> of_result >|= Entity.single)
  | _ -> None
;;

let predicate_of_yojson key_list (yojson : Yojson.Safe.t) =
  let open Entity in
  let open Helper in
  match yojson with
  | `Assoc assoc ->
    let open CCFun in
    let open CCOption in
    let go key of_yojson =
      assoc |> CCList.assoc_opt ~eq:CCString.equal key >>= of_yojson
    in
    let key = go key_string (Key.of_yojson %> of_result) >>= Key.to_human key_list in
    let operator = go operator_string (Operator.of_yojson %> of_result) in
    let value = go value_string value_of_yojson_opt in
    Predicate.create_human ?key ?operator ?value () |> CCResult.return
  | _ -> Error Pool_message.(Error.Invalid Field.Predicate)
;;

let rec of_yojson (key_list : Entity.Key.human list) json
  : (t, Pool_message.Error.t) result
  =
  let open CCResult in
  let error = Pool_message.(Error.Invalid Field.Query) in
  let of_yojson = of_yojson key_list in
  let not_empty l =
    match l with
    | [] -> Error Pool_message.Error.FilterAndOrMustNotBeEmpty
    | _ -> Ok l
  in
  let of_list to_predicate queries =
    queries
    |> not_empty
    >>= CCFun.(CCList.map of_yojson %> CCList.all_ok)
    >|= to_predicate
  in
  match json with
  | `Assoc [ (key, query) ] ->
    (match key, query with
     | "and", `List queries -> of_list (fun lst -> And lst) queries
     | "or", `List queries -> of_list (fun lst -> Or lst) queries
     | "not", f -> f |> of_yojson >|= fun p -> Not p
     | "pred", p -> p |> predicate_of_yojson key_list >|= fun p -> Pred p
     | "template", id ->
       let id =
         match id with
         | `String id -> id |> Pool_common.Id.of_string |> CCOption.pure
         | _ -> None
       in
       Ok (Template id)
     | _ -> Error error)
  | _ -> Error error
;;

let all_in_query_fcn fcn (query : t) =
  let open Entity.Predicate in
  let rec find = function
    | And queries | Or queries -> CCList.flat_map find queries
    | Not p -> find p
    | Pred { key; value; _ } -> fcn (key, value)
    | Template _ -> []
  in
  query |> find
;;

let[@warning "-4"] all_query_experiments =
  let open Entity.Key in
  all_in_query_fcn (function
    | Some (Hardcoded Participation), Some (Entity.Lst lst) ->
      lst |> Filter_utils.single_val_to_id
    | _, _ -> [])
;;

let[@warning "-4"] all_query_tags =
  let open Entity.Key in
  all_in_query_fcn (function
    | Some (Hardcoded Tag), Some (Entity.Lst lst) ->
      lst |> Filter_utils.single_val_to_id |> CCList.map Tags.Id.of_common
    | _, _ -> [])
;;
