module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.create
          `Version
          (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let announcement permission id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    one_of_tuple (permission, `Version, Some target_id)
  ;;

  let index_permission = Read
  let index = one_of_tuple (index_permission, `Version, None)
  let create = one_of_tuple (Create, `Version, None)
  let read = announcement Read
  let update = announcement Update
end
