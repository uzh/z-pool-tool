module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "custom_field_settings.cqrs"

let custom_field_effect action id =
  let open Guard in
  let target_id = id |> Guard.Uuid.target_of Custom_field.Id.value in
  ValidationSet.one_of_tuple (action, `CustomField, Some target_id)
;;

type command =
  { field_type : Custom_field.FieldType.t
  ; required : Custom_field.Required.t
  ; disabled : Custom_field.Disabled.t
  ; custom_field_group_id : Custom_field.Group.Id.t option
  ; admin_hint : Custom_field.AdminHint.t option
  ; admin_override : Custom_field.AdminOverride.t
  ; admin_view_only : Custom_field.AdminViewOnly.t
  ; admin_input_only : Custom_field.AdminInputOnly.t
  ; prompt_on_registration : Custom_field.PromptOnRegistration.t
  }

let base_command
  field_type
  required
  disabled
  custom_field_group_id
  admin_hint
  admin_override
  admin_view_only
  admin_input_only
  prompt_on_registration
  =
  { field_type
  ; required
  ; disabled
  ; custom_field_group_id
  ; admin_hint
  ; admin_override
  ; admin_view_only
  ; admin_input_only
  ; prompt_on_registration
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
        ; AdminOverride.schema ()
        ; AdminViewOnly.schema ()
        ; AdminInputOnly.schema ()
        ; PromptOnRegistration.schema ()
        ]
      base_command)
;;

let base_decode data =
  Conformist.decode_and_validate base_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module UpdateVisibilitySettings : sig
  include Common.CommandSig with type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> session_close:Custom_field.t list
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) ~session_close =
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
        admin_override
        admin_view_only
        admin_input_only
        prompt_on_registration
    in
    Ok [ Custom_field.Created t |> Pool_event.custom_field ]
  ;;

  let effects = Custom_field.Guard.Access.create
end
