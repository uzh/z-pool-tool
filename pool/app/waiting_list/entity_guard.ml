module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton `WaitingList)
          `WaitingList
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;

  let to_authorizable_of_repo ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Repo_entity.{ id; _ } ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton `WaitingList)
          `WaitingList
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
