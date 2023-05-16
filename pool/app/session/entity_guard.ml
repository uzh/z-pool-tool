open Utils.Lwt_result.Infix

let relation ?ctx () =
  let open Guard in
  let to_target =
    Relation.Query.create Repo.Sql.find_binary_experiment_id_sql
  in
  Persistence.Relation.add ?ctx ~to_target ~target:`Experiment `Session
;;

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Target.make `Session (id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet

  let session action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    One (action, TargetSpec.Id (`Session, target_id))
  ;;

  let index id =
    And
      [ One (Action.Read, TargetSpec.Entity `Session)
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ One (Action.Create, TargetSpec.Entity `Session)
      ; Experiment.Guard.Access.update id
      ]
  ;;

  let read experiment_id session_id =
    And
      [ session Action.Read session_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id session_id =
    And
      [ session Action.Update session_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete experiment_id session_id =
    And
      [ session Action.Delete session_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;
end
