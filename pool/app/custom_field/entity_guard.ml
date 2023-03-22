module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun custom_field ->
        let id = Entity.id custom_field |> Uuid.target_of Entity.Id.value in
        Target.make `CustomField id)
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module Group = struct
  module Target = struct
    type t = Entity.t [@@deriving eq, show]

    let to_authorizable ?ctx t =
      let open Guard in
      Persistence.Target.decorate
        ?ctx
        (fun { Entity.Group.id; _ } ->
          Target.make `CustomField (id |> Uuid.target_of Entity.Group.Id.value))
        t
      |> Lwt_result.map_error Pool_common.Message.authorization
    ;;
  end
end
