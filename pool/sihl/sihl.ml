(* This is provided by Sihl *)

let todo = failwith "todo"

module User = struct
  type t = { id : string }

  let activate _ = todo
  let deactivate _ = todo
end

type timestamp = unit
