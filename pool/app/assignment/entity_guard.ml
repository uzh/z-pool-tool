let target_of = Guard.Uuid.target_of Entity.Id.value

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } -> Target.create `Assignment (id |> target_of))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let assignment action uuid = One (action, uuid |> target_of |> id)

  let index id =
    And [ One (Read, Model `Assignment); Experiment.Guard.Access.read id ]
  ;;

  let create id =
    And [ One (Create, Model `Assignment); Experiment.Guard.Access.read id ]
  ;;

  let read experiment_id assignment_id =
    And
      [ assignment Read assignment_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id assignment_id =
    And
      [ assignment Update assignment_id
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;

  let delete experiment_id assignment_id =
    And
      [ assignment Delete assignment_id
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;

  let deleted = index
end
