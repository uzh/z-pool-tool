module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "custom_field.cqrs"

type command =
  { field_type : Custom_field.FieldType.t
  ; required : Custom_field.Required.t
  ; disabled : Custom_field.Disabled.t
  ; custom_field_group_id : Custom_field.Group.Id.t option
  ; admin_hint : Custom_field.AdminHint.t option
  ; admin_overwrite : Custom_field.AdminOverwrite.t
  ; admin_view_only : Custom_field.AdminViewOnly.t
  ; admin_input_only : Custom_field.AdminInputOnly.t
  }

let base_command
  field_type
  required
  disabled
  custom_field_group_id
  admin_hint
  admin_overwrite
  admin_view_only
  admin_input_only
  =
  { field_type
  ; required
  ; disabled
  ; custom_field_group_id
  ; admin_hint
  ; admin_overwrite
  ; admin_view_only
  ; admin_input_only
  }
;;

let base_schema =
  let open Custom_field in
  Pool_common.Utils.PoolConformist.(
    make
      Field.
        [ FieldType.schema ()
        ; Required.schema ()
        ; Disabled.schema ()
        ; Conformist.optional @@ Group.Id.schema ()
        ; Conformist.optional @@ AdminHint.schema ()
        ; AdminOverwrite.schema ()
        ; AdminViewOnly.schema ()
        ; AdminInputOnly.schema ()
        ]
      base_command)
;;

let base_decode data =
  Conformist.decode_and_validate base_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module Create : sig
  include Common.CommandSig with type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Custom_field.Id.t
    -> Pool_common.Language.t list
    -> Custom_field.Model.t
    -> (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> (string * string) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t = command

  let handle
    ?(tags = Logs.Tag.empty)
    ?id
    sys_languages
    model
    name
    hint
    validation
    { field_type
    ; required
    ; disabled
    ; custom_field_group_id
    ; admin_hint
    ; admin_overwrite
    ; admin_view_only
    ; admin_input_only
    }
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let* hint = Custom_field.Hint.create hint in
    let* t =
      Custom_field.create
        ?id
        field_type
        model
        name
        hint
        validation
        required
        disabled
        custom_field_group_id
        admin_hint
        admin_overwrite
        admin_view_only
        admin_input_only
    in
    Ok [ Custom_field.Created t |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `TargetEntity `CustomField ]
end

module Update : sig
  include Common.CommandSig with type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_common.Language.t list
    -> Custom_field.t
    -> (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> (string * string) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Custom_field.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = command

  let handle
    ?(tags = Logs.Tag.empty)
    sys_languages
    custom_field
    name
    hint
    validation
    { field_type
    ; required
    ; disabled
    ; custom_field_group_id
    ; admin_hint
    ; admin_overwrite
    ; admin_view_only
    ; admin_input_only
    }
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let* hint = Custom_field.Hint.create hint in
    let id = Custom_field.id custom_field in
    let* () =
      if Custom_field.FieldType.equal
           field_type
           (Custom_field.field_type custom_field)
         || CCOption.is_none Custom_field.(published_at custom_field)
      then Ok ()
      else Error Pool_common.Message.CustomFieldTypeChangeNotAllowed
    in
    let* t =
      Custom_field.create
        ~id
        field_type
        Custom_field.(model custom_field)
        name
        hint
        validation
        required
        disabled
        custom_field_group_id
        admin_hint
        admin_overwrite
        admin_view_only
        admin_input_only
    in
    Ok [ Custom_field.Updated t |> Pool_event.custom_field ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Custom_field.Id.value)
    ; `Update, `TargetEntity `CustomField
    ]
  ;;
end

module Sort : sig
  include Common.CommandSig with type t = Custom_field.t list

  val effects : Custom_field.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Custom_field.t list

  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Sort" ~tags);
    Ok [ Custom_field.FieldsSorted t |> Pool_event.custom_field ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Custom_field.Id.value)
    ; `Update, `TargetEntity `CustomField
    ]
  ;;
end

module Publish : sig
  include Common.CommandSig with type t = Custom_field.t

  val effects : Custom_field.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Custom_field.t

  let handle ?(tags = Logs.Tag.empty) m =
    Logs.info ~src (fun m -> m "Handle command Publish" ~tags);
    Ok [ Custom_field.Published m |> Pool_event.custom_field ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Custom_field.Id.value)
    ; `Update, `TargetEntity `CustomField
    ]
  ;;
end

module Delete : sig
  include Common.CommandSig with type t = Custom_field.t

  val effects : Custom_field.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Custom_field.t

  let handle ?(tags = Logs.Tag.empty) m =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    match Custom_field.published_at m with
    | None -> Ok [ Custom_field.Deleted m |> Pool_event.custom_field ]
    | Some _ -> Error Pool_common.Message.(AlreadyPublished Field.CustomField)
  ;;

  let effects id =
    [ `Delete, `Target (id |> Guard.Uuid.target_of Custom_field.Id.value)
    ; `Delete, `TargetEntity `CustomField
    ]
  ;;
end
