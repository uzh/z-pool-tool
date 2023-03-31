module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.make
          `Filter
          (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let filter action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Filter, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `Filter)
  let create = One (Action.Read, TargetSpec.Entity `Filter)
  let read = filter Action.Read
  let update = filter Action.Update
  let delete = filter Action.Delete
end
