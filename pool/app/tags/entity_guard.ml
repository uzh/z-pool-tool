open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Tag (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let tag action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetEntity.Id target_id)
  ;;

  let index = One (Update, TargetEntity.Model `Tag)
  let create = One (Create, TargetEntity.Model `Tag)
  let read_entity = One (Read, TargetEntity.Model `Tag)
  let read = tag Read
  let update = tag Update
  let delete = tag Delete
  let assign access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
  let remove access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
end
