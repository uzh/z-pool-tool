module Actor : sig
  include Guardian.RoleSig with type t = Entity.Actor.t

  val of_string_res : string -> (t, Pool_common.Message.error) result
  val can_assign_roles : t -> t list
  val equal_or_nil_target : t -> t -> bool
  val has_nil_target : t -> bool
  val find_target_of : t -> t -> Guardian.Contract.Uuid.Target.t option
  val update_target : t -> Guardian.Contract.Uuid.Target.t -> t

  type input_type =
    | QueryExperiments
    | QueryLocations

  val pp_input_type : Format.formatter -> input_type -> unit
  val show_input_type : input_type -> string
  val equal_input_type : input_type -> input_type -> bool
  val type_of_key : t -> (input_type option, Pool_common.Message.error) result
  val key_to_string : t -> string
end

module Target : sig
  include Guardian.RoleSig with type t = Entity.Target.t
end
