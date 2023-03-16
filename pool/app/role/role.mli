module Actor : Guardian.RoleSig with type t = Entity.Actor.t

module Target : sig
  include Guardian.RoleSig with type t = Entity.Target.t

  type admins = Entity.Target.admins

  val all_admins : Entity.Target.t list
  val all_entities : Entity.Target.t list
end
