module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun t ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton `Assignment)
          `Assignment
          (t.Entity.id
           |> Pool_common.Id.value
           |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
