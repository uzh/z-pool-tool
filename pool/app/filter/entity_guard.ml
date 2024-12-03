module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun { Entity.id; _ } ->
         Guard.Target.create
           `Filter
           (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let filter ?id ?(model = `Filter) permission =
    one_of_tuple
      (permission, model, CCOption.map (Uuid.target_of Entity.Id.value) id)
  ;;

  let index = one_of_tuple (Read, `Filter, None)

  let create ?experiment_id () =
    CCOption.map_or
      ~default:(filter Create)
      (fun id ->
         And
           [ Or [ filter Create; filter ~id:(Entity.Id.of_common id) Create ] ])
      experiment_id
  ;;

  let update id = filter ~id Update
  let delete id = filter ~id Delete
end
