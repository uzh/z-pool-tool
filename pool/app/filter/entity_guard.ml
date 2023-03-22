module Target = struct
  let (_ : (unit, string) result) =
    let open Utils.Lwt_result.Infix in
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `Filter
        `Experiment
        (fun pool id ->
          Repo.find_experiment_id_opt pool id
          ||> CCOption.to_result "Result is hidden")
        Entity.Id.of_string
        Pool_common.Id.value
    in
    Guard.Persistence.Dependency.register
      ~parent:`Experiment
      `Filter
      find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun (t : t) ->
        Guard.Target.make
          `Filter
          (t.Entity.id
           |> Pool_common.Id.value
           |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
