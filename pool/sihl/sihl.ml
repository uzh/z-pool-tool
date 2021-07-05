(* This is provided by Sihl and can be removed later on, use it for TDD *)

let todo = failwith "todo"

module User = struct
  type t = { id : string }

  let set_password _ _ = todo
  let activate _ = todo
  let deactivate _ = todo
  let create _ = todo
end

type timestamp = unit
