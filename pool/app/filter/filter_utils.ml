(** TODO[timhub]:

    * enum is not supported for constructors without arguments, any other way?

    * Do we need translations? *)

type filter_label =
  | And
  | Or
  | Not
  | Pred
[@@deriving eq, enum, show]

let stringify_label = function
  | And -> "and", "And"
  | Or -> "or", "Or"
  | Not -> "not", "Not"
  | Pred -> "pred", "Predicate"
;;

let label_of_string = function
  | "and" -> Ok And
  | "or" -> Ok Or
  | "not" -> Ok Not
  | "pred" -> Ok Pred
  | _ -> Error Pool_common.Message.(Invalid Field.Predicate)
;;

let default_filter_label = And

let all_filter_labels : filter_label list =
  CCList.range min_filter_label max_filter_label
  |> CCList.map filter_label_of_enum
  |> CCList.all_some
  |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
;;
