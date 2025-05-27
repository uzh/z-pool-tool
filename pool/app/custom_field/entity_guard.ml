open Utils.Lwt_result.Infix
open Guard

type group_id = Entity.Group.Id.t

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun field ->
         Target.create `CustomField (field |> Entity.id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Group = struct
  module Target = struct
    type t = Entity.t [@@deriving eq, show]

    let to_authorizable ?ctx t =
      Persistence.Target.decorate
        ?ctx
        (fun { Entity.Group.id; _ } ->
           Guard.Target.create `CustomField (id |> Uuid.target_of Entity.Group.Id.value))
        t
      >|- Pool_message.Error.authorization
    ;;
  end
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let custom_field action uuid =
    one_of_tuple (action, `CustomField, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index = one_of_tuple (Read, `CustomField, None)
  let create = one_of_tuple (Create, `CustomField, None)
  let update = custom_field Update
  let delete = custom_field Delete

  module Group = struct
    let group action uuid =
      one_of_tuple
        (action, `CustomFieldGroup, Some (uuid |> Uuid.target_of Entity.Group.Id.value))
    ;;

    let index = one_of_tuple (Read, `CustomFieldGroup, None)
    let create = one_of_tuple (Create, `CustomFieldGroup, None)
    let update = group Update
    let delete = group Delete
  end
end
