module Target = struct
  let (_ : (unit, string) result) =
    let open Utils.Lwt_result.Infix in
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `Session
        `Experiment
        (fun pool id -> Repo.find_experiment_id_and_title pool id >|+ fst)
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Guard.Persistence.Dependency.register `Session `Experiment find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make
          `Session
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
