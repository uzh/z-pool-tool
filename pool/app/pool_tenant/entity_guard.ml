open Utils.Lwt_result.Infix

module Actor = struct
  type t = Entity.t [@@deriving show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Actor.decorate
      ?ctx
      (fun ({ Entity.id; _ } : t) ->
        Actor.create `System (id |> Uuid.actor_of Pool_common.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Target = struct
  type t = Entity.t

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun ({ Entity.id; _ } : t) ->
        Target.create `Tenant (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let tenant action id =
    one_of_tuple (action, `Tenant, Some (id |> Uuid.target_of Entity.Id.value))
  ;;

  let index = one_of_tuple (Read, `Tenant, None)
  let create = one_of_tuple (Create, `Tenant, None)
  let read = tenant Read
  let update = tenant Update
  let delete = tenant Delete
end
