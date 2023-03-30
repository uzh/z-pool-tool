module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.make
          `MessageTemplate
          (id |> Entity.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let read id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (Action.Read, TargetSpec.Id (`MessageTemplate, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `MessageTemplate)
end
