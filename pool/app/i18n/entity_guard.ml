module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make `I18n (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `I18n)

  let read id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (Action.Read, TargetSpec.Id (`I18n, target_id))
  ;;
end
