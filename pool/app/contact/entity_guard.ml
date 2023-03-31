open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun user ->
        Target.make
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
    Persistence.Actor.decorate
      ?ctx
      (encode %> Actor.make (RoleSet.singleton `Contact) `Contact)
      id
    >|- Format.asprintf "Failed to convert Contact to authorizable: %s"
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx =
    let encode = Entity.id %> Uuid.actor_of Pool_common.Id.value in
    decorate ?ctx encode
  ;;

  (** Many request handlers do not extract a [User.t] at any point. This
      function is useful in such cases. *)
  let authorizable_of_req ?ctx req =
    Sihl.Web.Session.find "user_id" req
    |> CCResult.of_opt
    |> Lwt_result.lift
    >|- Pool_common.Message.authorization
    >>= decorate ?ctx Uuid.Actor.of_string_exn
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let contact action id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (action, TargetSpec.Id (`Contact, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `Assignment)
  let create = One (Action.Create, TargetSpec.Entity `Contact)
  let read = contact Action.Read
  let update = contact Action.Update
end
