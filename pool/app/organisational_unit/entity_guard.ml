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
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let organisational_unit action uuid =
    one_of_tuple
      ( action
      , `OrganisationalUnit
      , Some (uuid |> Uuid.target_of Entity.Id.value) )
  ;;

  let index = one_of_tuple (Read, `OrganisationalUnit, None)
  let create = one_of_tuple (Create, `OrganisationalUnit, None)
  let update = organisational_unit Update
  let delete = organisational_unit Delete
end
