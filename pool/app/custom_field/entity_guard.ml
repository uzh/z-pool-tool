open Utils.Lwt_result.Infix
open Guard

type group_id = Entity.Group.Id.t

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun field ->
        Target.make
          `CustomField
          (field |> Entity.id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Group = struct
  module Target = struct
    type t = Entity.t [@@deriving eq, show]

    let to_authorizable ?ctx t =
      Persistence.Target.decorate
        ?ctx
        (fun { Entity.Group.id; _ } ->
          Guard.Target.make
            `CustomField
            (id |> Uuid.target_of Entity.Group.Id.value))
        t
      >|- Pool_common.Message.authorization
    ;;
  end
end

module Access = struct
  open Guard
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `CustomField)

  let read id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (Action.Read, TargetSpec.Id (`CustomField, target_id))
  ;;

  module Group = struct
    let index = One (Action.Read, TargetSpec.Entity `CustomFieldGroup)

    let read id =
      let target_id = id |> Uuid.target_of Entity.Group.Id.value in
      One (Action.Read, TargetSpec.Id (`CustomFieldGroup, target_id))
    ;;
  end
end
