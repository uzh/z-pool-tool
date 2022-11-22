module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Actor.decorate
      ?ctx
      (fun t ->
        Guard.Authorizable.make
          (Guard.ActorRoleSet.singleton `Admin)
          `Admin
          (t
          |> Entity.user
          |> fun Sihl_user.{ id; _ } -> id |> Guard.Uuid.Actor.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx role t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun t ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton (`Admin role))
          (`Admin role)
          (t
          |> Entity.user
          |> fun Sihl_user.{ id; _ } -> id |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
