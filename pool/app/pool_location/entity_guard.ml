open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

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
  let (_ : (unit, string) result) =
    let find_parent =
      Utils.create_simple_dependency_with_pool
        `LocationFile
        `Location
        Repo.RepoFileMapping.find_location_id
        Pool_common.Id.of_string
        Entity.Id.value
    in
    Persistence.Dependency.register ~parent:`Location `LocationFile find_parent
  ;;

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

  let index = One (Action.Read, TargetSpec.Entity `Location)

  let read id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (Action.Read, TargetSpec.Id (`Location, target_id))
  ;;

  module File = struct
    let index = One (Action.Read, TargetSpec.Entity `LocationFile)

    let read id =
      let target_id = id |> Uuid.target_of Entity.Mapping.Id.value in
      One (Action.Read, TargetSpec.Id (`LocationFile, target_id))
    ;;
  end
end
