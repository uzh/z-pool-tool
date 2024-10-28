open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Target.create `Location (id |> Uuid.target_of Entity.Id.value))
      t
    >|- Pool_message.Error.authorization
  ;;
end

module FileTarget = struct
  type t = Entity.Mapping.file [@@deriving eq, show]

  let decorate ?ctx id =
    Persistence.Target.decorate
      ?ctx
      (Uuid.target_of Entity.Mapping.Id.value
       %> Guard.Target.create `LocationFile)
      id
    >|- Pool_message.Error.authorization
  ;;

  let to_authorizable ?ctx { Entity.Mapping.id; _ } = decorate ?ctx id

  let to_authorizable_of_write ?ctx { Entity.Mapping.Write.id; _ } =
    decorate ?ctx id
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let location ?(model = `Location) action uuid =
    one_of_tuple (action, model, Some (uuid |> Uuid.target_of Entity.Id.value))
  ;;

  let index_permission = Read
  let index = one_of_tuple (index_permission, `Location, None)
  let create = one_of_tuple (Create, `Location, None)
  let read ?model = location ?model Read
  let update ?model = location ?model Update
  let delete ?model = location ?model Delete

  module File = struct
    let file action uuid =
      one_of_tuple
        ( action
        , `LocationFile
        , Some (uuid |> Uuid.target_of Entity.Mapping.Id.value) )
    ;;

    let index = one_of_tuple (Read, `LocationFile, None)
    let create = one_of_tuple (Create, `LocationFile, None)
    let read = file Read
    let update = file Update
    let delete = file Delete
  end
end
