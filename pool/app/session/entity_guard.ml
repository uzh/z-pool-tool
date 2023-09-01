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

  let index_permission = Read

  let session permission uuid =
    one_of_tuple
      (permission, `Session, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index id =
    And
      [ one_of_tuple (index_permission, `Session, None)
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ one_of_tuple (Create, `Session, None)
      ; Experiment.Guard.Access.update id
      ]
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
