open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.make `Tag (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let tag action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Tag, target_id))
  ;;

  let index = One (Action.Update, TargetSpec.Entity `Tag)
  let create = One (Action.Create, TargetSpec.Entity `Tag)
  let read_entity = One (Action.Read, TargetSpec.Entity `Tag)
  let read = tag Action.Read
  let update = tag Action.Update
  let delete = tag Action.Delete
  let assign access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
  let remove access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
end
