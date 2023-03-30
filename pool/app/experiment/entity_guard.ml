module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.make `Experiment (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let target_of = Uuid.target_of Entity.Id.value
  let index = One (Action.Read, TargetSpec.Entity `Experiment)
  let read id = One (Action.Read, TargetSpec.Id (`Experiment, target_of id))
  let update id = One (Action.Read, TargetSpec.Id (`Experiment, target_of id))
end
