module Target = struct
  let (_ : (unit, string) result) =
    let open Utils.Lwt_result.Infix in
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `Invitation
        `Experiment
        (fun pool id ->
          Repo.find pool id >>= Repo.find_experiment_id_of_invitation pool)
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Guard.Persistence.Dependency.register
      ~parent:`Experiment
      `Invitation
      find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make
          `Invitation
          (id |> Pool_common.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
