module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "i18n.cqrs"

module Create : sig
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

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects tenant =
    [ ( `Update
      , `Target
          (tenant.Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value) )
    ]
  ;;
end

module Update : sig
  type t = { content : I18n.Content.t }

  val handle
    :  ?tags:Logs.Tag.set
    -> I18n.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects tenant =
    [ ( `Update
      , `Target
          (tenant.Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value) )
    ]
  ;;
end
