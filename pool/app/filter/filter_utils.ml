let print = Utils.ppx_printer

type filter_label =
  | And [@printer print "and"]
  | Or [@printer print "or"]
  | Not [@printer print "not"]
  | Pred [@printer print "pred"]
  | Template [@printer print "template"]
[@@deriving eq, enum, show]

let to_label = function
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"
  | Pred -> "Predicate"
  | Template -> "Template"
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
