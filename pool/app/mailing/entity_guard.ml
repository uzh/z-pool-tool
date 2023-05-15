let relation ?ctx () =
  let open Guard in
  let to_target =
    Relation.Query.create Repo.Sql.find_binary_experiment_id_sql
  in
  Persistence.Relation.add ?ctx ~to_target ~target:`Experiment `Mailing
;;

module Target = struct
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

  let mailing action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Mailing, target_id))
  ;;

  let index id =
    And
      [ One (Action.Read, TargetSpec.Entity `Mailing)
      ; Experiment.Guard.Access.read id
      ; Experiment.Guard.Access.recruiter_of id
      ]
  ;;

  let create id =
    And
      [ One (Action.Create, TargetSpec.Entity `Mailing)
      ; Experiment.Guard.Access.update id
      ; Experiment.Guard.Access.recruiter_of id
      ]
  ;;

  let read experiment_id mailing_id =
    And
      [ mailing Action.Read mailing_id
      ; Experiment.Guard.Access.read experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;

  let update experiment_id mailing_id =
    And
      [ mailing Action.Update mailing_id
      ; Experiment.Guard.Access.read experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;

  let delete experiment_id mailing_id =
    And
      [ mailing Action.Delete mailing_id
      ; Experiment.Guard.Access.delete experiment_id
      ; Experiment.Guard.Access.recruiter_of experiment_id
      ]
  ;;
end
