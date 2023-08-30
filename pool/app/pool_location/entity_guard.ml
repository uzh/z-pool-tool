open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Location (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module FileTarget = struct
  type t = Entity.Mapping.file [@@deriving eq, show]

  let decorate ?ctx id =
    Persistence.Target.decorate
      ?ctx
      (Uuid.target_of Entity.Mapping.Id.value
       %> Guard.Target.create `LocationFile)
      id
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx { Entity.Mapping.id; _ } = decorate ?ctx id

  let to_authorizable_of_write ?ctx { Entity.Mapping.Write.id; _ } =
    decorate ?ctx id
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let location action uuid =
    One (action, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index = One (Read, Model `Location)
  let create = One (Create, Model `Location)
  let read = location Read
  let update = location Update
  let delete = location Delete

  module File = struct
    let file action uuid =
      One (action, uuid |> Uuid.target_of Entity.Mapping.Id.value |> id)
    ;;

    let index = One (Read, Model `LocationFile)
    let create = One (Create, Model `LocationFile)
    let read = file Read
    let update = file Update
    let delete = file Delete
  end
end
