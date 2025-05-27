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
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let invitation action uuid =
    one_of_tuple (action, `Invitation, Some (uuid |> Uuid.target_of Pool_common.Id.value))
  ;;

  let index id =
    And
      [ Or
          [ one_of_tuple (Read, `Invitation, None)
          ; one_of_tuple (Read, `Invitation, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ Or
          [ one_of_tuple (Create, `Invitation, None)
          ; one_of_tuple
              (Create, `Invitation, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.update id
      ]
  ;;

  let read experiment_id invitation_id =
    And
      [ Or
          [ invitation Read invitation_id
          ; one_of_tuple
              (Read, `Invitation, Some (Uuid.target_of Experiment.Id.value experiment_id))
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id invitation_id =
    And
      [ Or
          [ invitation Update invitation_id
          ; one_of_tuple
              ( Update
              , `Invitation
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete experiment_id invitation_id =
    And
      [ Or
          [ invitation Delete invitation_id
          ; one_of_tuple
              ( Delete
              , `Invitation
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;
end
