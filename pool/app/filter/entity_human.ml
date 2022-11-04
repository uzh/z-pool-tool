let print = Entity.print

type t =
  | And of t list [@printer print "and"]
  | Or of t list [@printer print "or"]
  | Not of t [@printer print "not"]
  | Pred of Entity.Predicate.human [@printer print "pred"]
[@@deriving show { with_path = false }]

let init ?key ?operator ?value () : t =
  Pred (Entity.Predicate.create_human ?key ?operator ?value ())
;;

let rec of_yojson (key_list : Entity.Key.human list) json
  : (t, Pool_common.Message.error) result
  =
  let open CCResult in
  let error = Pool_common.Message.(Invalid Field.Filter) in
  let of_yojson = of_yojson key_list in
  let of_list to_predicate filters =
    filters |> CCList.map of_yojson |> CCList.all_ok >|= to_predicate
  in
  match json with
  | `Assoc [ (key, filter) ] ->
    (match key, filter with
     | "and", `List filters -> of_list (fun lst -> And lst) filters
     | "or", `List filters -> of_list (fun lst -> Or lst) filters
     | "not", f -> f |> of_yojson >|= fun p -> Not p
     | "pred", p ->
       p |> Entity.Predicate.human_of_yojson key_list >|= fun p -> Pred p
     | _ -> Error error)
  | _ -> Error error
;;
