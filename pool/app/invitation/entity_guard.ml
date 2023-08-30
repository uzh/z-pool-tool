module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create
          `Invitation
          (id |> Pool_common.Id.value |> Uuid.Target.of_string_exn))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let invitation action uuid =
    One (action, uuid |> Uuid.target_of Pool_common.Id.value |> id)
  ;;

  let index id =
    And [ One (Read, Model `Invitation); Experiment.Guard.Access.read id ]
  ;;

  let create id =
    And [ One (Create, Model `Invitation); Experiment.Guard.Access.update id ]
  ;;

  let read experiment_id invitation_id =
    And
      [ invitation Read invitation_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id invitation_id =
    And
      [ invitation Update invitation_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete experiment_id invitation_id =
    And
      [ invitation Delete invitation_id
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;
end
