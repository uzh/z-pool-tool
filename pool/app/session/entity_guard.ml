open Utils.Lwt_result.Infix

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
        Target.create `Session (id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission
  open TargetEntity

  let index_permission = Read

  let session permission uuid =
    One (permission, uuid |> Uuid.target_of Entity.Id.value |> id)
  ;;

  let index id =
    And
      [ One (index_permission, Model `Session)
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And [ One (Create, Model `Session); Experiment.Guard.Access.update id ]
  ;;

  let read experiment_id session_id =
    And [ session Read session_id; Experiment.Guard.Access.read experiment_id ]
  ;;

  let update experiment_id session_id =
    And
      [ session Update session_id; Experiment.Guard.Access.read experiment_id ]
  ;;

  let delete experiment_id session_id =
    And
      [ session Delete session_id; Experiment.Guard.Access.read experiment_id ]
  ;;
end
