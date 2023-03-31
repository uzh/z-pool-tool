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

  let setting action = One (action, TargetSpec.Entity `SystemSetting)
  let index = setting Action.Read
  let create = setting Action.Create
  let read = setting Action.Read
  let update = setting Action.Update
  let delete = setting Action.Delete
end
