include Entity
include Event
module Guard = Entity_guard
module Human = Entity_human
module VersionHistory = Version_history
module UtilsF = Filter_utils

let find = Repo.find
let find_all_templates = Repo.find_all_templates
let find_templates_by = Repo.find_templates_by
let find_template = Repo.find_template
let find_templates_of_query = Repo.find_templates_of_query
let find_multiple_templates = Repo.find_multiple_templates
let find_filtered_contacts = Repo.find_filtered_contacts
let count_filtered_contacts = Repo.count_filtered_contacts
let contact_matches_filter = Repo.contact_matches_filter

module Repo = struct
  let sql_select_columns = Repo.sql_select_columns
  let t = Repo_entity.t
  let query = Repo_entity.Query.t
end

let all_keys tenant_db =
  let open Utils.Lwt_result.Infix in
  let open Custom_field in
  find_by_model tenant_db Model.Contact
  ||> CCList.map Key.customfield
  ||> CCList.append Key.(all_hardcoded |> CCList.map hardcoded)
;;

(* TODO [aerben] also replace all Lwt.Syntax *)
let key_of_string tenant_db str =
  let open Utils.Lwt_result.Infix in
  let open Key in
  match Key.read str with
  | Some hardcoded -> (Hardcoded hardcoded : human) |> Lwt_result.return
  | None ->
    (str
     |> Custom_field.Id.of_string
     |> Custom_field.find tenant_db
     >|+ fun field : human -> CustomField field)
;;

let rec t_to_human key_list subquery_list (t : query) =
  let t_to_human = t_to_human key_list subquery_list in
  match t with
  | And predicates -> Human.And (predicates |> CCList.map t_to_human)
  | Or predicates -> Human.Or (predicates |> CCList.map t_to_human)
  | Not p -> Human.Not (t_to_human p)
  | Pred { Predicate.key; operator; value } ->
    Human.Pred
      Predicate.
        { key = Key.to_human key_list key; operator = Some operator; value = Some value }
  | Template filter_id ->
    subquery_list
    |> CCList.find_opt (fun filter -> Pool_common.Id.equal filter.id filter_id)
    |> CCOption.map (fun s -> s.id)
    |> fun id -> Human.Template id
;;

let toggle_predicate_type (filter : Human.t) predicate_type =
  let open Human in
  let empty = Entity.Predicate.create_human ?key:None ?operator:None in
  let filter_list () =
    match filter with
    | And lst | Or lst -> lst
    | Not f -> [ f ]
    | Pred s -> [ Pred s ]
    | Template id -> [ Template id ]
  in
  let rec find_predicate (query : Human.t) =
    match query with
    | And lst | Or lst ->
      lst
      |> CCList.head_opt
      |> CCOption.map find_predicate
      |> CCOption.value ~default:(empty ())
    | Not f -> find_predicate f
    | Pred s -> s
    | Template _ -> empty ()
  in
  match predicate_type with
  | "and" -> Ok (Human.And (filter_list ()))
  | "or" -> Ok (Or (filter_list ()))
  | "not" -> Ok (Not (Pred (find_predicate filter)))
  | "pred" -> Ok (Pred (find_predicate filter) : t)
  | "template" -> Ok (Template None)
  | _ -> Error Pool_message.(Error.Invalid Field.Filter)
;;

let all_in_query_fcn fcn { query; _ } =
  let rec find = function
    | And queries | Or queries -> CCList.flat_map find queries
    | Not p -> find p
    | Pred { Predicate.key; value; _ } -> fcn (key, value)
    | Template _ -> []
  in
  query |> find
;;

let[@warning "-4"] all_query_experiments =
  let open Entity.Key in
  all_in_query_fcn (function
    | Hardcoded Participation, Entity.Lst lst -> lst |> Filter_utils.single_val_to_id
    | _, _ -> [])
;;

let[@warning "-4"] all_query_tags =
  let open Entity.Key in
  all_in_query_fcn (function
    | Hardcoded Tag, Entity.Lst lst ->
      lst |> Filter_utils.single_val_to_id |> CCList.map Tags.Id.of_common
    | _, _ -> [])
;;
