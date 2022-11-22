module Actor : Guardian.RoleSig with type t = Entity.Actor.t

module Target : sig
  include Guardian.RoleSig with type t = Entity.Target.t

  val all_entities : Entity.Target.t list
end
