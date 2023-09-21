module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Experiment (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let experiment ?(model = `Experiment) permission uuid =
    one_of_tuple
      (permission, model, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index_permission = Read
  let index = one_of_tuple (index_permission, `Experiment, None)
  let create = one_of_tuple (Create, `Experiment, None)
  let read ?model = experiment ?model Read
  let update ?model = experiment ?model Update
  let delete ?model = experiment ?model Delete
end
