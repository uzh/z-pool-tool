module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "i18n.cqrs"

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

  val effects : Pool_common.Id.t -> Guard.ValidationSet.t
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

  let effects id =
    let open Guard in
    let i18n_id = id |> Uuid.target_of Pool_common.Id.value in
    ValidationSet.(And [ One (Action.Update, TargetSpec.Id (`I18n, i18n_id)) ])
  ;;
end
