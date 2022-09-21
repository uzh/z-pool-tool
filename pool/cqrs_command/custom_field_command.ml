module Conformist = Pool_common.Utils.PoolConformist

type command =
  { model : Custom_field.Model.t
  ; field_type : Custom_field.FieldType.t
  ; validation : Custom_field.Validation.t
  ; required : Custom_field.Required.t
  ; disabled : Custom_field.Disabled.t
  ; admin : Custom_field.Admin.t
  }

let base_command
  model
  field_type
  validation_regex
  validation_error
  required
  disabled
  admin_hint
  admin_overwrite
  =
  let validation =
    Custom_field.Validation.
      { regex = validation_regex; error = validation_error }
  in
  let admin =
    Custom_field.Admin.{ hint = admin_hint; overwrite = admin_overwrite }
  in
  { model; field_type; validation; required; disabled; admin }
;;

let base_schema =
  let open Custom_field in
  Pool_common.Utils.PoolConformist.(
    make
      Field.
        [ Model.schema ()
        ; FieldType.schema ()
        ; Validation.Regex.schema ()
        ; Validation.Error.schema ()
        ; Required.schema ()
        ; Disabled.schema ()
        ; Conformist.optional @@ Admin.Hint.schema ()
        ; Admin.Overwrite.schema ()
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
    -> (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle
    ?id
    names
    hints
    { model; field_type; validation; required; disabled; admin }
    =
    let open CCResult in
    let* t =
      Custom_field.create
        ?id
        model
        names
        hints
        field_type
        validation
        required
        disabled
        admin
    in
    Ok [ Custom_field.Created t |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end

module Update : sig
  type t = command

  val handle
    :  (Pool_common.Language.t * string) list
    -> (Pool_common.Language.t * string) list
    -> Custom_field.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = command

  let handle
    name
    hint
    custom_field
    { model; field_type; validation; required; disabled; admin }
    =
    let open CCResult in
    let* name = Custom_field.Name.create name in
    let* hint = Custom_field.Hint.create hint in
    let t =
      (Custom_field.
         { custom_field with
           model
         ; name
         ; hint
         ; field_type
         ; validation
         ; required
         ; disabled
         ; admin
         }
        : Custom_field.t)
    in
    Ok [ Custom_field.Updated t |> Pool_event.custom_field ]
  ;;

  let effects = [ `Create, `Role `Admin ]
end
