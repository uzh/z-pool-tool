module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.create
          `Filter
          (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let filter action uuid =
    one_of_tuple (action, `Filter, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index = one_of_tuple (Read, `Filter, None)
  let create = one_of_tuple (Read, `Filter, None)
  let update = filter Update
  let delete = filter Delete
end
