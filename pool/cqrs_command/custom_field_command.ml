module Conformist = Pool_common.Utils.PoolConformist

type command =
  { field_type : Custom_field.FieldType.t
  ; required : Custom_field.Required.t
  ; disabled : Custom_field.Disabled.t
  ; custom_field_group_id : Custom_field.Group.Id.t option
  ; admin_hint : Custom_field.Admin.Hint.t option
  ; admin_overwrite : Custom_field.Admin.Overwrite.t
  ; admin_view_only : Custom_field.Admin.ViewOnly.t
  ; admin_input_only : Custom_field.Admin.InputOnly.t
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
        ; Conformist.optional @@ Admin.Hint.schema ()
        ; Admin.Overwrite.schema ()
        ; Admin.ViewOnly.schema ()
        ; Admin.InputOnly.schema ()
        ]
      base_command)
;;

let base_decode data =
  Conformist.decode_and_validate base_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module Create : sig
  type t = command

  val handle
    :  ?id:Custom_field.Id.t
    -> Pool_common.Language.t list
    -> Custom_field.Model.t
    -> (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> (string * string) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle
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
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let* hint = Custom_field.Hint.create hint in
    let* admin =
      Custom_field.Admin.create
        admin_hint
        admin_overwrite
        admin_view_only
        admin_input_only
    in
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
        admin
    in
    Ok [ Custom_field.Created t |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Update : sig
  type t = command

  val handle
    :  Pool_common.Language.t list
    -> Custom_field.t
    -> (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> (string * string) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle
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
    let open CCResult in
    let* name = Custom_field.Name.create sys_languages name in
    let* hint = Custom_field.Hint.create hint in
    let* admin =
      Custom_field.Admin.create
        admin_hint
        admin_overwrite
        admin_view_only
        admin_input_only
    in
    let id = Custom_field.id custom_field in
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
        admin
    in
    Ok [ Custom_field.Updated t |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Sort : sig
  type t = Custom_field.t list

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = Custom_field.t list

  let handle t = Ok [ Custom_field.FieldsSorted t |> Pool_event.custom_field ]
  let effects = [ `Create, `Role `Admin ]
end
