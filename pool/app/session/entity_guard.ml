open Utils.Lwt_result.Infix

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx id =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun id ->
        Target.create `Session (id |> Uuid.target_of Pool_common.Id.value))
      id
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let index_permission = Read

  let session ?session_id ?(model = `Session) permission =
    one_of_tuple
      ( permission
      , model
      , CCOption.map (Uuid.target_of Entity.Id.value) session_id )
  ;;

  let index ?(model = `Session) id =
    And
      [ Or
          [ session ~model index_permission
          ; Experiment.Guard.Access.read ~model id
          ]
      ; Experiment.Guard.Access.read id
      ]
  ;;

  let create ?model id =
    And
      [ Or
          [ session ?model Create
          ; one_of_tuple
              ( Create
              , CCOption.value ~default:`Session model
              , Some (Uuid.target_of Experiment.Id.value id) )
          ]
      ; Experiment.Guard.Access.update id
      ]
  ;;

  let read ?(model = `Session) experiment_id session_id =
    Or
      [ session ~model ~session_id Read
      ; Experiment.Guard.Access.read ~model experiment_id
      ]
  ;;

  let read_by_location ?(model = `Session) location_id session_id =
    Or
      [ session ~model ~session_id Read
      ; Pool_location.Guard.Access.read ~model location_id
      ]
  ;;

  let update ?(model = `Session) experiment_id session_id =
    And
      [ Or
          [ session ~model ~session_id Update
          ; Experiment.Guard.Access.update ~model experiment_id
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let delete ?(model = `Session) experiment_id session_id =
    And
      [ Or
          [ session ~model ~session_id Delete
          ; Experiment.Guard.Access.delete ~model experiment_id
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;

  let close experiment_id session_id =
    And
      [ Or
          [ session ~model:`Session ~session_id Update
          ; session ~model:`SessionClose ~session_id Update
          ; Experiment.Guard.Access.update ~model:`Session experiment_id
          ; Experiment.Guard.Access.update ~model:`SessionClose experiment_id
          ]
      ; Experiment.Guard.Access.read experiment_id
      ]
  ;;
end
