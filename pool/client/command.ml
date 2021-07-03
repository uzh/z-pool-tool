type add_client_command = { client : Entity.client }
type handle_add_client = add_client_command -> (Event.t list, string) Result.t

module AddClient : sig
  type t

  val handle : t -> (Event.t list, string) Result.t
end = struct
  type t

  let handle = Sihl.todo
end

module EditClient : sig
  type t

  val handle : t -> Entity.client -> (Event.t list, string) Result.t
end = struct
  type t

  let handle _ _ = Sihl.todo
end

type operator = unit

module AddOperator : sig
  type t

  val handle : t -> Entity.client -> (Event.t list, string) Result.t
end = struct
  type t

  let handle _ _ = Sihl.todo
end

module ActivateOperator : sig
  type t

  val handle : t -> operator -> (Event.t list, string) Result.t
end = struct
  type t

  let handle _ _ = Sihl.todo
end

module DeactivateOperator : sig
  type t

  val handle : t -> operator -> (Event.t list, string) Result.t
end = struct
  type t

  let handle _ _ = Sihl.todo
end
