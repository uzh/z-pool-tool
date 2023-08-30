open Utils.Lwt_result.Infix
open Guard

type group_id = Entity.Group.Id.t

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun field ->
        Target.create
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
          Guard.Target.create
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
  open Permission
  open TargetEntity

  let custom_field action uuid =
    One (action, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index = One (Read, Model `CustomField)
  let create = One (Create, Model `CustomField)
  let update = custom_field Update
  let delete = custom_field Delete

  module Group = struct
    let group action uuid =
      One (action, uuid |> Uuid.target_of Entity.Group.Id.value |> id)
    ;;

    let index = One (Read, Model `CustomFieldGroup)
    let create = One (Create, Model `CustomFieldGroup)
    let update = group Update
    let delete = group Delete
  end
end
