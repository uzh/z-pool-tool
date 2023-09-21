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

  let assignment action uuid =
    one_of_tuple (action, `Assignment, Some (uuid |> target_of))
  ;;

  let index id =
    And
      [ Or
          [ one_of_tuple (Read, `Assignment, None)
          ; one_of_tuple
              (Read, `Assignment, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ Or
          [ one_of_tuple (Create, `Assignment, None)
          ; one_of_tuple
              (Create, `Assignment, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let read experiment_id assignment_id =
    And
      [ Or
          [ assignment Read assignment_id
          ; one_of_tuple
              ( Read
              , `Assignment
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id assignment_id =
    And
      [ Or
          [ assignment Update assignment_id
          ; one_of_tuple
              ( Update
              , `Assignment
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete experiment_id assignment_id =
    And
      [ Or
          [ assignment Delete assignment_id
          ; one_of_tuple
              ( Delete
              , `Assignment
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let deleted = index
end
