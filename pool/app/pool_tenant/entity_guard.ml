open Utils.Lwt_result.Infix

module Actor = struct
  type t = Entity.t [@@deriving show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Actor.decorate
      ?ctx
      (fun ({ Entity.id; _ } : t) ->
        Actor.make
          (RoleSet.singleton `System)
          `System
          (id |> Uuid.actor_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun ({ Entity.id; _ } : t) ->
        Target.make `Tenant (id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module SmtpTarget = struct
  type t = Entity.SmtpAuth.t

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun ({ Entity.SmtpAuth.id; _ } : Entity.SmtpAuth.t) ->
        Target.make `Smtp (id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let tenant action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Tenant, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `Tenant)
  let create = One (Action.Create, TargetSpec.Entity `Tenant)
  let update = tenant Action.Update
  let delete = tenant Action.Delete

  module Smtp = struct
    let smtp action id =
      let target_id = id |> Uuid.target_of Entity.SmtpAuth.Id.value in
      One (action, TargetSpec.Id (`Smtp, target_id))
    ;;

    let index = One (Action.Read, TargetSpec.Entity `Smtp)
    let create = One (Action.Create, TargetSpec.Entity `Smtp)
    let read = smtp Action.Read
    let update = smtp Action.Update
    let delete = smtp Action.Delete
  end
end
