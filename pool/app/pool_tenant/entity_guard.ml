module Actor = struct
  type t = Entity.t [@@deriving show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Actor.decorate
      ?ctx
      (fun (t : t) ->
        Authorizable.make
          (ActorRoleSet.singleton `System)
          `Tenant
          (Uuid.Actor.of_string_exn (Pool_common.Id.value t.Entity.id)))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun (t : t) ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton `Tenant)
          `Tenant
          (Guard.Uuid.Target.of_string_exn (Pool_common.Id.value t.Entity.id)))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
