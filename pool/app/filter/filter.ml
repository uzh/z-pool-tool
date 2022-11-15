include Entity
include Event

let find = Repo.find
let find_all_subfilters = Repo.find_all_subfilters
let find_subfilter = Repo.find_subfilter
let find_multiple_subfilters = Repo.find_multiple_subfilters

module Human = struct
  include Entity_human
end

module Utils = struct
  include Filter_utils
end

module Repo = struct
  let t = Repo_entity.t
end

let all_keys tenant_db =
  let open Lwt.Infix in
  let open Custom_field in
  find_by_model tenant_db Model.Contact
  >|= CCList.map Key.customfield
  >|= CCList.append Key.(all_hardcoded |> CCList.map hardcoded)
;;

let key_of_string tenant_db str =
  let open Lwt_result.Infix in
  let open Key in
  match Key.read str with
  | Some hardcoded -> (Hardcoded hardcoded : human) |> Lwt_result.return
  | None ->
    str
    |> Custom_field.Id.of_string
    |> Custom_field.find tenant_db
    >|= fun field : human -> CustomField field
;;

let rec t_to_human key_list subfilter_list (t : filter) =
  let t_to_human = t_to_human key_list subfilter_list in
  match t with
  | And predicates -> Human.And (predicates |> CCList.map t_to_human)
  | Or predicates -> Human.Or (predicates |> CCList.map t_to_human)
  | Not p -> Human.Not (t_to_human p)
  | Pred { Predicate.key; operator; value } ->
    Human.Pred
      Predicate.
        { key = Key.to_human key_list key
        ; operator = Some operator
        ; value = Some value
        }
  | SubFilter filter_id ->
    subfilter_list
    |> CCList.find_opt (fun filter -> Pool_common.Id.equal filter.id filter_id)
    |> CCOption.map (fun s -> s.id)
    |> fun id -> Human.SubFilter id
;;

let toggle_predicate_type (filter : Human.t) predicate_type =
  let open Human in
  let empty = Entity.Predicate.create_human ?key:None ?operator:None in
  let filter_list () =
    match filter with
    | And lst | Or lst -> lst
    | Not f -> [ f ]
    | Pred s -> [ Pred s ]
    | SubFilter id -> [ SubFilter id ]
  in
  let rec find_predicate (filter : Human.t) =
    match filter with
    | And lst | Or lst ->
      lst
      |> CCList.head_opt
      |> CCOption.map find_predicate
      |> CCOption.value ~default:(empty ())
    | Not f -> find_predicate f
    | Pred s -> s
    | SubFilter _ -> empty ()
  in
  match predicate_type with
  | "and" -> Ok (Human.And (filter_list ()))
  | "or" -> Ok (Or (filter_list ()))
  | "not" -> Ok (Not (Pred (find_predicate filter)))
  | "pred" -> Ok (Pred (find_predicate filter) : t)
  | "sub_filter" -> Ok (SubFilter None)
  | _ -> Error Pool_common.Message.(Invalid Field.Filter)
;;

let rec search_subfilters ids filter =
  let search_list filters ids =
    CCList.fold_left
      (fun ids filter -> search_subfilters ids filter)
      ids
      filters
  in
  match filter with
  | And lst | Or lst -> search_list lst ids
  | Not f -> search_subfilters ids f
  | Pred _ -> ids
  | SubFilter id -> id :: ids
;;

(* TODO: Probably not necessary to pass filter query, just pass entire filter *)
let find_subfilters_of_filter tenant_db ?exclude filter =
  let open Lwt.Infix in
  let rec go filters ids subfilters =
    match filters with
    | [] -> subfilters |> Lwt.return
    | _ ->
      let new_ids = CCList.flat_map (search_subfilters []) filters in
      CCList.filter
        (fun id -> Stdlib.not (CCList.mem ~eq:Pool_common.Id.equal id ids))
        new_ids
      |> find_multiple_subfilters ?exclude tenant_db
      >>= fun filter_list ->
      go
        (filter_list |> CCList.map (fun f -> f.filter))
        (ids @ new_ids)
        (subfilters @ filter_list)
  in
  go [ filter ] [] []
;;
