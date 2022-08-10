(** TODO[timhub]:

    * enum is not supported for constructors without arguments, any other way?

    * Do we need translations? *)

type filter_label =
  | And
  | Or
  | Not
  | PredS
  | PredM
[@@deriving eq, enum, show]

let stringify_label = function
  | And -> "and", "And"
  | Or -> "or", "Or"
  | Not -> "not", "Not"
  | PredS -> "pred_s", "Single predicate"
  | PredM -> "pred_m", "Multi predicate"
;;

let label_of_string = function
  | "and" -> Ok And
  | "or" -> Ok Or
  | "not" -> Ok Not
  | "pred_s" -> Ok PredS
  | "pred_m" -> Ok PredM
  | _ -> Error Pool_common.Message.(Invalid Field.Predicate)
;;

let default_filter_label = And

let all_filter_labels : filter_label list =
  CCList.range min_filter_label max_filter_label
  |> CCList.map filter_label_of_enum
  |> CCList.all_some
  |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
;;

(* TODO[timhub]: Can i use single or multi? *)

let all_single_operators : [> `Multi | `Single ] Entity.Operator.t list =
  let open Entity.Operator in
  [ Less; LessEqual; Greater; GreaterEqual; Equal; NotEqual ]
;;

let all_multi_operators : [> `Multi | `Single ] Entity.Operator.t list =
  let open Entity.Operator in
  [ ContainsSome; ContainsNone; ContainsAll ]
;;

let input_type_to_operator =
  let open Entity.Operator in
  function
  | `Bool -> [ Equal; NotEqual ]
  | `Str -> [ Equal; NotEqual ]
  | `Date | `Nr -> [ Equal; NotEqual; Greater; GreaterEqual; Less; LessEqual ]
;;
