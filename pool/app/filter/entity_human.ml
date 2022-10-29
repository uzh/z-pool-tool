open Entity

type t =
  | And of t * t [@printer print_filter "and"]
  | Or of t * t [@printer print_filter "or"]
  | Not of t [@printer print_filter "not"]
  (* TODO[timhub]: Fix this type *)
  | PredS of [ `Single | `Multi ] Predicate.human
      [@printer print_filter "pred_s"]
  | PredM of [ `Single | `Multi ] Predicate.human
      [@printer print_filter "pred_m"]
[@@deriving show { with_path = false }]

let of_string =
  let pred = Entity.Predicate.create_human ?key:None ?operator:None in
  let as_t () : t = PredS (pred ()) in
  function
  | "and" -> Ok (And (as_t (), as_t ()) : t)
  | "or" -> Ok (Or (as_t (), as_t ()) : t)
  | "not" -> Ok (Not (as_t ()) : t)
  | "pred_s" -> Ok (PredS (pred ()) : t)
  | "pred_m" -> Ok (PredM (pred ()) : t)
  | _ -> Error Pool_common.Message.(Invalid Field.Predicate)
;;
