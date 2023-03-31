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

  let custom_field action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`CustomField, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `CustomField)
  let create = One (Action.Create, TargetSpec.Entity `CustomField)
  let read = custom_field Action.Read
  let update = custom_field Action.Update
  let delete = custom_field Action.Delete

  module Group = struct
    let group action id =
      let target_id = id |> Uuid.target_of Entity.Group.Id.value in
      One (action, TargetSpec.Id (`CustomFieldGroup, target_id))
    ;;

    let index = One (Action.Read, TargetSpec.Entity `CustomFieldGroup)
    let create = One (Action.Create, TargetSpec.Entity `CustomFieldGroup)
    let read = group Action.Read
    let update = group Action.Update
    let delete = group Action.Delete
  end
end
