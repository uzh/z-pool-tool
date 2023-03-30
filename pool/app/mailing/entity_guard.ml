module Target = struct
  let (_ : (unit, string) result) =
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `Mailing
        `Experiment
        Repo.find_experiment_id
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Guard.Persistence.Dependency.register
      ~parent:`Experiment
      `Mailing
      find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make `Mailing (id |> Guard.Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let read_mailing id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (Action.Read, TargetSpec.Id (`Mailing, target_id))
  ;;

  let recruiter_of_experiment id =
    let target_id = id |> Uuid.target_of Experiment.Id.value in
    Or [ SpecificRole (`Recruiter target_id); SpecificRole `RecruiterAll ]
  ;;

  let index id =
    And
      [ One (Action.Read, TargetSpec.Entity `Mailing)
      ; Experiment.Guard.Access.read id
      ; recruiter_of_experiment id
      ]
  ;;

  let read experiment_id mailing_id =
    And
      [ read_mailing mailing_id
      ; Experiment.Guard.Access.read experiment_id
      ; recruiter_of_experiment experiment_id
      ]
  ;;
end
