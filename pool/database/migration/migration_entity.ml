exception Exception of string
exception Dirty_migration

module Step = struct
  type t =
    { label : string
    ; statement : string
    ; check_fk : bool
    }
  [@@deriving eq, show]

  let to_sexp { label; statement; check_fk } =
    let open Sexplib0.Sexp_conv in
    let open Sexplib0.Sexp in
    List
      [ List [ Atom "label"; sexp_of_string label ]
      ; List [ Atom "statement"; sexp_of_string statement ]
      ; List [ Atom "check_fk"; sexp_of_bool check_fk ]
      ]
  ;;

  let create ~label ?(check_fk = true) statement = { label; check_fk; statement }
end

type steps = Step.t list [@@deriving eq, show]
type t = string * steps [@@deriving eq, show]

let to_sexp (namespace, steps) =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  let steps = CCList.map Step.to_sexp steps in
  List (CCList.cons (List [ Atom "namespace"; sexp_of_string namespace ]) steps)
;;

let pp fmt t = Sexplib0.Sexp.pp_hum fmt (to_sexp t)
let empty namespace = namespace, []

(* Append the migration step to the list of steps *)
let add_step step (label, steps) = label, CCList.concat [ steps; [ step ] ]
