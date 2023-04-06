module Actor : sig
  include Guardian.RoleSig with type t = Entity.Actor.t

  val can_assign_roles : t -> t list
  val equal_or_nil_target : t -> t -> bool
end

module Target : sig
  include Guardian.RoleSig with type t = Entity.Target.t
end
