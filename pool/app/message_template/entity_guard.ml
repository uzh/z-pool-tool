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
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let message_template action uuid =
    one_of_tuple
      (action, `MessageTemplate, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index = one_of_tuple (Read, `MessageTemplate, None)
  let create = one_of_tuple (Create, `MessageTemplate, None)
  let update = message_template Update
  let delete = message_template Delete
end
