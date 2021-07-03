(* This is provided by Sihl *)

let todo = failwith "todo"

module User = struct
  type t

  let activate _ = todo
  let deactivate _ = todo
end

type timestamp = unit
type 'a io = unit
