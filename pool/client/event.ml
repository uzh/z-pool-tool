type t =
  [ `Added of Entity.client
  | `Edited of Entity.client
  | `Operator_added of Entity.client * Sihl.User.t
  ]
