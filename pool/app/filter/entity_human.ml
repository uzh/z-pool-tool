open Entity

type t =
  | And of t list [@printer print "and"]
  | Or of t list [@printer print "or"]
  | Not of t [@printer print "not"]
  (* TODO[timhub]: Fix this type *)
  | Pred of Predicate.human [@printer print "pred"]
[@@deriving show { with_path = false }]

(* TODO: Remove *)
let of_string =
  let pred = Entity.Predicate.create_human ?key:None ?operator:None in
  let as_t () : t = Pred (pred ()) in
  function
  | "and" -> Ok (And [ as_t (); as_t () ] : t)
  | "or" -> Ok (Or [ as_t (); as_t () ] : t)
  | "not" -> Ok (Not (as_t ()) : t)
  | "pred" -> Ok (Pred (pred ()) : t)
  | _ -> Error Pool_common.Message.(Invalid Field.Predicate)
;;
