open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Tag (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let tag action id =
    let target_id = id |> Uuid.target_of Entity.Id.value in
    one_of_tuple (action, `Tag, Some target_id)
  ;;

  let index = one_of_tuple (Update, `Tag, None)
  let create = one_of_tuple (Create, `Tag, None)
  let read_entity = one_of_tuple (Read, `Tag, None)
  let read = tag Read
  let update = tag Update
  let delete = tag Delete
  let assign access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
  let remove access_fcn model_uuid = And [ read_entity; access_fcn model_uuid ]
end
