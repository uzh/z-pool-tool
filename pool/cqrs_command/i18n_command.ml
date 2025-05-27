module Conformist = Pool_conformist

let src = Logs.Src.create "i18n.cqrs"

module Update : sig
  include Common.CommandSig

  type t = I18n.Content.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?system_event_id:System_event.Id.t
    -> I18n.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t = I18n.Content.t

  let schema = Conformist.(make Field.[ I18n.Content.schema () ] CCFun.id)

  let handle ?(tags = Logs.Tag.empty) ?system_event_id property (command : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      [ I18n.Updated (property, command) |> Pool_event.i18n
      ; System_event.(
          Job.I18nPageUpdated
          |> create ?id:system_event_id
          |> created
          |> Pool_event.system_event)
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = I18n.Guard.Access.update
end
