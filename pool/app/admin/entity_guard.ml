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
       %> (fun { Sihl_user.id; _ } -> id |> Uuid.Actor.of_string_exn)
       %> Actor.create `Admin)
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (Entity.user
       %> (fun { Sihl_user.id; _ } -> id |> Uuid.Target.of_string_exn)
       %> Target.create `Admin)
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open ValidationSet
  open Permission
  open TargetEntity

  let index = One (Read, Model `Admin)
  let create = One (Create, Model `Admin)
  let read id = One (Read, Id (target_of id))
  let update id = One (Update, Id (target_of id))
end
