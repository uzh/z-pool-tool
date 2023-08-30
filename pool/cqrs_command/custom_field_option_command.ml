module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "custom_field_option_command.cqrs"

type command = (Pool_common.Language.t * string) list

let custom_field_effect action id =
  let open Guard in
  let target_id = id |> Guard.Uuid.target_of Custom_field.Id.value in
  ValidationSet.One (action, TargetEntity.Id target_id)
;;

module Create : sig
  include Common.CommandSig with type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Custom_field.SelectOption.Id.t
    -> Pool_common.Language.t list
    -> Custom_field.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) ?id sys_languages field name =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let m = Custom_field.SelectOption.create ?id name in
    Ok Custom_field.[ OptionCreated (id field, m) |> Pool_event.custom_field ]
  ;;

  let effects = Custom_field.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_common.Language.t list
    -> Custom_field.SelectOption.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Custom_field.Id.t -> Guard.ValidationSet.t
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) sys_languages option command =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open Custom_field in
    let open CCResult in
    let* name = Name.create sys_languages command in
    let m = SelectOption.create ~id:option.SelectOption.id name in
    Ok [ OptionUpdated m |> Pool_event.custom_field ]
  ;;

  let effects = Custom_field.Guard.Access.update
end

module Destroy : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Custom_field.SelectOption.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Custom_field.Id.t -> Guard.ValidationSet.t
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) option =
    Logs.info ~src (fun m -> m "Handle command Destroy" ~tags);
    match option.Custom_field.SelectOption.published_at with
    | None ->
      Ok [ Custom_field.OptionDestroyed option |> Pool_event.custom_field ]
    | Some _ ->
      Error Pool_common.Message.(AlreadyPublished Field.CustomFieldOption)
  ;;

  let effects = Custom_field.Guard.Access.delete
end

module Publish : sig
  include Common.CommandSig with type t = Custom_field.SelectOption.t

  val effects : Custom_field.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Custom_field.SelectOption.t

  let handle ?(tags = Logs.Tag.empty) m =
    Logs.info ~src (fun m -> m "Handle command Publish" ~tags);
    Ok [ Custom_field.OptionPublished m |> Pool_event.custom_field ]
  ;;

  let effects = Custom_field.Guard.Access.update
end

module Sort : sig
  include Common.CommandSig with type t = Custom_field.SelectOption.t list

  val effects : Custom_field.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Custom_field.SelectOption.t list

  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Publish" ~tags);
    Ok [ Custom_field.OptionsSorted t |> Pool_event.custom_field ]
  ;;

  let effects = Custom_field.Guard.Access.update
end
