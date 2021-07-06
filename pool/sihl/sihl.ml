(* This is provided by Sihl and can be removed later on, use it for TDD *)

let todo _ = failwith "todo"

module User = struct
  type t = { id : string } [@@deriving eq, show]

  let set_password _ = todo
  let activate = todo
  let deactivate = todo
  let create ~email:_ ~password:_ = todo ()
end

module Database = struct
  let with_transaction _ = todo
end

type timestamp = unit [@@deriving eq, show]

let now () = ()
