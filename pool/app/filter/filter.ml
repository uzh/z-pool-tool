include Entity
include Event

let find = Repo.find
let find_all_components = Repo.find_all_components
let find_component = Repo.find_component

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

let rec t_to_human key_list (t : filter) =
  let t_to_human = t_to_human key_list in
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
;;

let toggle_predicate_type (filter : Human.t) predicate_type =
  let open Human in
  let empty = Entity.Predicate.create_human ?key:None ?operator:None in
  let filter_list () =
    match filter with
    | And lst | Or lst -> lst
    | Not f -> [ f ]
    | Pred s -> [ Pred s ]
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
  in
  match predicate_type with
  | "and" -> Ok (Human.And (filter_list ()))
  | "or" -> Ok (Or (filter_list ()))
  | "not" -> Ok (Not (Pred (find_predicate filter)))
  | "pred" -> Ok (Pred (find_predicate filter) : t)
  | _ -> Error Pool_common.Message.(Invalid Field.Filter)
;;
