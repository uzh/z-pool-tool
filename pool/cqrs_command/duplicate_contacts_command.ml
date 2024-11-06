open Duplicate_contacts

let src = Logs.Src.create "duplicate_contacts.cqrs"

module Ignore : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Ignore" ~tags);
    Ok [ Ignored t |> Pool_event.duplicate_contacts ]
  ;;
end
