module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "custom_field_group.cqrs"

type name_command = (Pool_common.Language.t * string) list

let custom_field_group_effect action id =
  let open Guard in
  let target_id = id |> Guard.Uuid.target_of Custom_field.Group.Id.value in
  ValidationSet.One (action, TargetSpec.Id (`CustomFieldGroup, target_id))
;;

module Create : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Custom_field.Group.Id.t
    -> Pool_common.Language.t list
    -> name_command
    -> Custom_field.Model.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) ?id sys_languages name model =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let group = Custom_field.Group.create ?id model name in
    Ok Custom_field.[ GroupCreated group |> Pool_event.custom_field ]
  ;;

  let effects =
    let open Guard in
    ValidationSet.One (Action.Create, TargetSpec.Entity `CustomField)
  ;;
end

module Update : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_common.Language.t list
    -> Custom_field.Group.t
    -> name_command
    -> Custom_field.Model.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Custom_field.Group.Id.t -> Guard.ValidationSet.t
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) sys_languages group names model =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let* names = Custom_field.Name.create sys_languages names in
    let group = Custom_field.Group.{ group with model; name = names } in
    Ok Custom_field.[ GroupUpdated group |> Pool_event.custom_field ]
  ;;

  let effects = custom_field_group_effect Guard.Action.Update
end

module Destroy : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Custom_field.Group.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Custom_field.Group.Id.t -> Guard.ValidationSet.t
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) option =
    Logs.info ~src (fun m -> m "Handle command Destroy" ~tags);
    Ok [ Custom_field.GroupDestroyed option |> Pool_event.custom_field ]
  ;;

  let effects = custom_field_group_effect Guard.Action.Delete
end

module Sort : sig
  include Common.CommandSig with type t = Custom_field.Group.t list

  val effects : Custom_field.Group.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Custom_field.Group.t list

  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Sort" ~tags);
    Ok [ Custom_field.GroupsSorted t |> Pool_event.custom_field ]
  ;;

  let effects = custom_field_group_effect Guard.Action.Update
end
