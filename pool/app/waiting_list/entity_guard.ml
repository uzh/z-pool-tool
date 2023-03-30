open CCFun.Infix
open Utils.Lwt_result.Infix

module Target = struct
  let (_ : (unit, string) result) =
    let open Guard in
    let find_parent =
      Utils.create_simple_dependency_with_pool
        `WaitingList
        `Experiment
        Repo.find_experiment_id
        Pool_common.Id.of_string
        Experiment.Id.value
    in
    Persistence.Dependency.register ~parent:`Experiment `WaitingList find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let decorate ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (Uuid.target_of Pool_common.Id.value %> Target.make `WaitingList)
      t
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx { Entity.id; _ } = decorate ?ctx id
  let to_authorizable_of_repo ?ctx { Repo_entity.id; _ } = decorate ?ctx id
end

module Access = struct
  open Guard
  open ValidationSet

  let read_waiting_list id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (Action.Read, TargetSpec.Id (`WaitingList, target_id))
  ;;

  let recruiter_of_experiment id =
    let target_id = id |> Uuid.target_of Experiment.Id.value in
    Or [ SpecificRole (`Recruiter target_id); SpecificRole `RecruiterAll ]
  ;;

  let index id =
    And
      [ One (Action.Read, TargetSpec.Entity `WaitingList)
      ; Experiment.Guard.Access.read id
      ; recruiter_of_experiment id
      ]
  ;;

  let read experiment_id waiting_list_id =
    And
      [ read_waiting_list waiting_list_id
      ; Experiment.Guard.Access.read experiment_id
      ; recruiter_of_experiment experiment_id
      ]
  ;;
end
