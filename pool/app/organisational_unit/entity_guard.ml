module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Guard.Target.make
          `OrganisationalUnit
          (id |> Entity.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let organisational_unit action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`OrganisationalUnit, target_id))
  ;;

  let index = One (Action.Read, TargetSpec.Entity `OrganisationalUnit)
  let create = One (Action.Create, TargetSpec.Entity `OrganisationalUnit)
  let update = organisational_unit Action.Update
  let delete = organisational_unit Action.Delete
end
