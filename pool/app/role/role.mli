module Actor : sig
  include Guardian.RoleSig with type t = Entity.Actor.t
end

module Role : sig
  include Guardian.RoleSig with type t = Entity.Role.t

  type input_type =
    | QueryExperiments
    | QueryLocations

  val pp_input_type : Format.formatter -> input_type -> unit
  val show_input_type : input_type -> string
  val equal_input_type : input_type -> input_type -> bool
  val type_of_key : t -> (input_type option, Pool_message.Error.t) result
  val of_name : string -> (t, Pool_message.Error.t) Result.t
  val customizable : t list
end

module Target : sig
  include Guardian.RoleSig with type t = Entity.Target.t

  val actor_permission : t list
  val of_name : string -> (t, Pool_message.Error.t) Result.t
  val static : t list
  val customizable : t list
end
