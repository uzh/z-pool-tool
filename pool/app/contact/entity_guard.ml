open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun user ->
        Target.create
          `Contact
          (user |> Entity.id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Format.asprintf "Failed to convert Contact to authorizable: %s"
    >|- Pool_common.Message.authorization
  ;;
end

module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let decorate ?ctx encode id =
    Persistence.Actor.decorate ?ctx (encode %> Actor.create `Contact) id
    >|- Format.asprintf "Failed to convert Contact to authorizable: %s"
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx =
    let encode = Entity.id %> Uuid.actor_of Pool_common.Id.value in
    decorate ?ctx encode
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let contact action uuid =
    One (action, uuid |> Uuid.target_of Pool_common.Id.value |> id)
  ;;

  let index = One (Read, Model `Contact)
  let create = One (Create, Model `Contact)
  let read = contact Read
  let update = contact Update
  let read_name = Or [ index; One (Read, Model `ContactName) ]
  let read_info = Or [ index; One (Read, Model `ContactInfo) ]
end
