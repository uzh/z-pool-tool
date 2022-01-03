module Create : sig
  type t =
    { key : I18n.Key.t
    ; language : Pool_common.Language.t
    ; content : I18n.Content.t
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Pool_database.Label.t -> 'admin Admin.t -> t -> bool Lwt.t
end = struct
  type t =
    { key : I18n.Key.t
    ; language : Pool_common.Language.t
    ; content : I18n.Content.t
    }

  let command key language content = { key; language; content }

  let schema =
    Conformist.(
      make
        Field.
          [ I18n.Key.schema ()
          ; Pool_common.Language.schema_i18n ()
          ; I18n.Content.schema ()
          ]
        command)
  ;;

  let handle (command : t) =
    let property : I18n.create =
      I18n.
        { key = command.key
        ; language = command.language
        ; content = command.content
        }
    in
    Ok [ I18n.Created property |> Pool_event.i18n ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can
      : type admin. Pool_database.Label.t -> admin Admin.t -> t -> bool Lwt.t
    =
   fun pool admin _ ->
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        (Admin.user admin)
        ~any_of:
          [ Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id) ]
    in
    pool
    |> Pool_tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
 ;;
end

module Update : sig
  type t = { content : I18n.Content.t }

  val handle
    :  I18n.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Pool_tenant.t -> bool Lwt.t
end = struct
  type t = { content : I18n.Content.t }

  let command content = { content }
  let schema = Conformist.(make Field.[ I18n.Content.schema () ] command)

  let handle property (command : t) =
    let edit : I18n.edit = I18n.{ content = command.content } in
    Ok [ I18n.Updated (property, edit) |> Pool_event.i18n ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can user (tenant : Pool_tenant.t) =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id) ]
  ;;
end
