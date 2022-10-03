module Conformist = Pool_common.Utils.PoolConformist

type command = (Pool_common.Language.t * string) list

module Create : sig
  type t = command

  val handle
    :  ?id:Custom_field.Id.t
    -> Pool_common.Language.t list
    -> Custom_field.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle ?id sys_languages field name =
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let m = Custom_field.SelectOption.create ?id name in
    Ok Custom_field.[ OptionCreated (id field, m) |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Update : sig
  type t = command

  val handle
    :  Pool_common.Language.t list
    -> Custom_field.SelectOption.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle sys_languages option command =
    let open Custom_field in
    let open CCResult in
    let* name = Name.create sys_languages command in
    let m = SelectOption.create ~id:option.SelectOption.id name in
    Ok [ OptionUpdated m |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Destroy : sig
  val handle
    :  Custom_field.SelectOption.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  let handle option =
    Ok [ Custom_field.OptionDestroyed option |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end
