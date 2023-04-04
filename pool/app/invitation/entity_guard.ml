module Target = struct
  let (_ : (unit, string) result) =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let find_parent =
      Utils.create_simple_dependency_with_pool
        `Invitation
        `Experiment
        (fun pool id ->
          Repo.find pool id >>= Repo.find_experiment_id_of_invitation pool)
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Persistence.Dependency.register ~parent:`Experiment `Invitation find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.make
          `Invitation
          (id |> Pool_common.Id.value |> Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let invitation action id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (action, TargetSpec.Id (`Invitation, target_id))
  ;;

  let index id =
    And
      [ One (Action.Read, TargetSpec.Entity `Invitation)
      ; Experiment.Guard.Access.read id
      ; Experiment.Guard.Access.recruiter_of id
      ]
  ;;

  let create id =
    And
      [ One (Action.Create, TargetSpec.Entity `Invitation)
      ; Experiment.Guard.Access.update id
      ; Experiment.Guard.Access.recruiter_of id
      ]
  ;;

  let read experiment_id invitation_id =
    And
      [ invitation Action.Read invitation_id
      ; Experiment.Guard.Access.read experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;

  let update experiment_id invitation_id =
    And
      [ invitation Action.Update invitation_id
      ; Experiment.Guard.Access.read experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;

  let delete experiment_id invitation_id =
    And
      [ invitation Action.Delete invitation_id
      ; Experiment.Guard.Access.update experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;
end
