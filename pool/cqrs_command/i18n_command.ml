module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "i18n.cqrs"

module Create : sig
  include Common.CommandSig

  type t =
    { key : I18n.Key.t
    ; language : Pool_common.Language.t
    ; content : I18n.Content.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.EffectSet.t
end = struct
  type t =
    { key : I18n.Key.t
    ; language : Pool_common.Language.t
    ; content : I18n.Content.t
    }

  let command key language content = { key; language; content }

  let schema =
    Pool_common.Utils.PoolConformist.(
      make
        Field.
          [ I18n.Key.schema ()
          ; Pool_common.Language.schema ()
          ; I18n.Content.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
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
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects id =
    let open Guard in
    let target_id = id |> Uuid.target_of Pool_tenant.Id.value in
    EffectSet.(
      And
        [ One (Action.Update, TargetSpec.Id (`Tenant, target_id))
        ; One (Action.Create, TargetSpec.Entity `I18n)
        ])
  ;;
end

module Update : sig
  include Common.CommandSig

  type t = { content : I18n.Content.t }

  val handle
    :  ?tags:Logs.Tag.set
    -> I18n.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Pool_common.Id.t -> Guard.EffectSet.t
end = struct
  type t = { content : I18n.Content.t }

  let command content = { content }
  let schema = Conformist.(make Field.[ I18n.Content.schema () ] command)

  let handle ?(tags = Logs.Tag.empty) property (command : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let edit : I18n.edit = I18n.{ content = command.content } in
    Ok [ I18n.Updated (property, edit) |> Pool_event.i18n ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects tenant_id id =
    let open Guard in
    let tenant_id = tenant_id |> Uuid.target_of Pool_tenant.Id.value in
    let i18n_id = id |> Uuid.target_of Pool_common.Id.value in
    (* TODO: [guardian] check if dependency registration makes tenant check
       obsolete *)
    EffectSet.(
      And
        [ One (Action.Update, TargetSpec.Id (`Tenant, tenant_id))
        ; One (Action.Update, TargetSpec.Id (`I18n, i18n_id))
        ])
  ;;
end
