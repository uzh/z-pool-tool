module Target = struct
  let (_ : (unit, string) result) =
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `WaitingList
        `Experiment
        Repo.find_experiment_id
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Guard.Persistence.Dependency.register
      ~parent:`Experiment
      `WaitingList
      find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make
          `WaitingList
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;

  let to_authorizable_of_repo ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Repo_entity.{ id; _ } ->
        Guard.Target.make
          `WaitingList
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
