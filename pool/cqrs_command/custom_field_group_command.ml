module Conformist = Pool_common.Utils.PoolConformist

type command = { model : Custom_field.Model.t }

let default_command model = { model }

let default_schema =
  let open Custom_field in
  Pool_common.Utils.PoolConformist.(
    make Field.[ Model.schema () ] default_command)
;;

type name_command = (Pool_common.Language.t * string) list

let default_decode data =
  Conformist.decode_and_validate default_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module Create : sig
  type t = command

  val handle
    :  ?id:Custom_field.Group.Id.t
    -> Pool_common.Language.t list
    -> name_command
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle ?id sys_languages name { model } =
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let group = Custom_field.Group.create ?id model name in
    Ok Custom_field.[ GroupCreated group |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Update : sig
  type t = command

  val handle
    :  Pool_common.Language.t list
    -> Custom_field.Group.t
    -> name_command
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle sys_languages group name { model } =
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let group = Custom_field.Group.{ group with model; name } in
    Ok Custom_field.[ GroupUpdated group |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Destroy : sig
  val handle
    :  Custom_field.Group.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  let handle option =
    Ok [ Custom_field.GroupDestroyed option |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end
