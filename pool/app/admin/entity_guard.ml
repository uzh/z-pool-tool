open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

let target_of = Uuid.target_of Entity.Id.value

module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Actor.decorate
      ?ctx
      (Entity.user
       %> (fun { Pool_user.id; _ } -> id |> Uuid.actor_of Pool_user.Id.value)
       %> Actor.create `Admin)
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (Entity.user
       %> (fun { Pool_user.id; _ } -> id |> Uuid.target_of Pool_user.Id.value)
       %> Target.create `Admin)
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open ValidationSet
  open Permission

  let index = one_of_tuple (Read, `Admin, None)
  let create = one_of_tuple (Create, `Admin, None)
  let read id = one_of_tuple (Read, `Admin, Some (target_of id))
  let update id = one_of_tuple (Update, `Admin, Some (target_of id))
end
