open CCFun.Infix
open Utils.Lwt_result.Infix

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let decorate ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (Uuid.target_of Pool_common.Id.value %> Target.create `WaitingList)
      t
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx { Entity.id; _ } = decorate ?ctx id
  let to_authorizable_of_repo ?ctx { Repo_entity.id; _ } = decorate ?ctx id
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let waiting_list action uuid =
    one_of_tuple
      (action, `WaitingList, Some (uuid |> Uuid.target_of Pool_common.Id.value))
  ;;

  let index id =
    And
      [ one_of_tuple (Read, `WaitingList, None)
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create id =
    And
      [ one_of_tuple (Create, `WaitingList, None)
      ; Experiment.Guard.Access.update id
      ]
  ;;

  let read experiment_id waiting_list_id =
    And
      [ waiting_list Read waiting_list_id
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let update experiment_id waiting_list_id =
    And
      [ waiting_list Update waiting_list_id
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;

  let delete experiment_id waiting_list_id =
    And
      [ waiting_list Delete waiting_list_id
      ; Experiment.Guard.Access.update experiment_id
      ]
  ;;
end
