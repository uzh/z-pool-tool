module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Experiment (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let experiment permission uuid =
    One (permission, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index_permission = Read
  let index = One (index_permission, Model `Experiment)
  let create = One (Create, Model `Experiment)
  let read = experiment Read
  let update = experiment Update
  let delete = experiment Delete
end
