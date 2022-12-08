module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Sihl_user.id; _ } ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton `CustomField)
          `CustomField
          (id |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
