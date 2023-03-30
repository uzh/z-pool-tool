open CCFun.Infix

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (Guard.Uuid.target_of Pool_common.Id.value
       %> Guard.Target.make `SystemSetting)
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `SystemSetting)

  let read id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (Action.Read, TargetSpec.Id (`SystemSetting, target_id))
  ;;
end
