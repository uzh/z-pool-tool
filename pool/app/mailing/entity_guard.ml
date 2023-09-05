module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.create `Mailing (id |> Guard.Uuid.target_of Entity.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let mailing action uuid =
    one_of_tuple
      (action, `WaitingList, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index id =
    And
      [ Or
          [ one_of_tuple (Read, `Mailing, None)
          ; one_of_tuple
              (Read, `Mailing, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ Or
          [ one_of_tuple (Create, `Mailing, None)
          ; one_of_tuple
              (Create, `Mailing, Some (Uuid.target_of Experiment.Id.value id))
          ]
      ; Experiment.Guard.Access.update id
      ]
  ;;

  let read experiment_id mailing_id =
    And
      [ Or
          [ mailing Read mailing_id
          ; one_of_tuple
              ( Read
              , `Mailing
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id mailing_id =
    And
      [ Or
          [ mailing Update mailing_id
          ; one_of_tuple
              ( Update
              , `Mailing
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete experiment_id mailing_id =
    And
      [ Or
          [ mailing Delete mailing_id
          ; one_of_tuple
              ( Delete
              , `Mailing
              , Some (Uuid.target_of Experiment.Id.value experiment_id) )
          ]
      ; Experiment.Guard.Access.delete experiment_id
      ]
  ;;
end
