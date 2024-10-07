module Conformist = Pool_conformist
open Api_key

let src = Logs.Src.create "api_key.cqrs"
let schema = Pool_conformist.(make Field.[ Name.schema () ] CCFun.id)

type command = Name.t

module Create : sig
  include Common.CommandSig

  type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> ?token:Token.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) ?id ?token name =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let api_key = create ?id ?token name in
    Ok [ Created api_key |> Pool_event.api_key ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.create
end

module Update : sig
  include Common.CommandSig

  type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> Api_key.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) api_key name =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok [ Updated (api_key, { api_key with name }) |> Pool_event.api_key ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.update
end
