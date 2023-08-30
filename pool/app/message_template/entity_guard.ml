module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.create
          `MessageTemplate
          (id |> Entity.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let message_template action uuid =
    One (action, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index = One (Read, Model `MessageTemplate)
  let create = One (Create, Model `MessageTemplate)
  let update = message_template Update
  let delete = message_template Delete
end
