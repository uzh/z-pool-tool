module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.create
          `OrganisationalUnit
          (id |> Entity.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let organisational_unit action uuid =
    One (action, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index = One (Read, Model `OrganisationalUnit)
  let create = One (Create, Model `OrganisationalUnit)
  let update = organisational_unit Update
  let delete = organisational_unit Delete
end
