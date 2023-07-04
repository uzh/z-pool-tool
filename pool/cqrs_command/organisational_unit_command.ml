module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "organisational_unit.cqrs"

let schema =
  Conformist.(make Field.[ Organisational_unit.Name.schema () ] CCFun.id)
;;

let decode data =
  Conformist.decode_and_validate schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module Create : sig
  type t = Organisational_unit.Name.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Organisational_unit.Id.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = Organisational_unit.Name.t

  let handle ?(tags = Logs.Tag.empty) ?id name =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let ou = Organisational_unit.create ?id name in
    Ok Organisational_unit.[ Created ou |> Pool_event.organisational_unit ]
  ;;

  let effects = Organisational_unit.Guard.Access.create
end

module Update : sig
  type t = Organisational_unit.Name.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Organisational_unit.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Organisational_unit.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Organisational_unit.Name.t

  let handle ?(tags = Logs.Tag.empty) ou name =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      Organisational_unit.
        [ Updated (ou, name) |> Pool_event.organisational_unit ]
  ;;

  let effects = Organisational_unit.Guard.Access.update
end
