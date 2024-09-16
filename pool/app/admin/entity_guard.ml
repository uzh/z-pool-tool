open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

let target_of = Uuid.target_of Entity.Id.value

let target_of_res string =
  try Ok (Uuid.target_of Entity.Id.value string) with
  | _ -> Error Pool_message.(Error.Invalid Field.Id)
;;

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
  open CCResult.Infix

  let index = one_of_tuple (Read, `Admin, None)
  let create = one_of_tuple (Create, `Admin, None)
  let read id = one_of_tuple (Read, `Admin, Some (target_of id))
  let update id = one_of_tuple (Update, `Admin, Some (target_of id))

  let read_res id =
    target_of_res id >|= fun id -> one_of_tuple (Read, `Admin, Some id)
  ;;

  let update_res id =
    target_of_res id >|= fun id -> one_of_tuple (Update, `Admin, Some id)
  ;;
end
