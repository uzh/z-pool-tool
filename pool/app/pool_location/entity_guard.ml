open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

let relation ?ctx () =
  let to_target =
    Relation.Query.create Repo.RepoFileMapping.Sql.find_binary_location_id_sql
  in
  Persistence.Relation.add ?ctx ~to_target ~target:`Location `LocationFile
;;

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.make `Location (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module FileTarget = struct
  type t = Entity.Mapping.file [@@deriving eq, show]

  let decorate ?ctx id =
    Persistence.Target.decorate
      ?ctx
      (Uuid.target_of Entity.Mapping.Id.value %> Guard.Target.make `LocationFile)
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

  let location action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Location, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `Location)
  let create = One (Action.Create, TargetSpec.Entity `Location)
  let read = location Action.Read
  let update = location Action.Update
  let delete = location Action.Delete

  module File = struct
    let file action id =
      let target_id = id |> Uuid.target_of Entity.Mapping.Id.value in
      One (action, TargetSpec.Id (`LocationFile, target_id))
    ;;

    let index = One (Action.Read, TargetSpec.Entity `LocationFile)
    let create = One (Action.Create, TargetSpec.Entity `LocationFile)
    let read = file Action.Read
    let update = file Action.Update
    let delete = file Action.Delete
  end
end
