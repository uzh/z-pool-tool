module Conformist = Pool_conformist
open Api_key

let src = Logs.Src.create "api_key.cqrs"

type create =
  { name : Name.t
  ; expires_at : ExpiresAt.t
  }

let create_schema =
  Pool_conformist.(
    make
      Field.[ Name.schema (); ExpiresAt.schema () ]
      (fun name expires_at -> { name; expires_at }))
;;

let update_schema = Pool_conformist.(make Field.[ Name.schema () ] CCFun.id)

type command = Name.t

module Create : sig
  include Common.CommandSig

  type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> ?token:Token.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = create

  let handle ?(tags = Logs.Tag.empty) ?id ?token ({ name; expires_at } : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let api_key = create ?id ?token name expires_at in
    Ok [ Created api_key |> Pool_event.api_key ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate create_schema data
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
    Pool_conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.update
end

module Disable : sig
  type t = Api_key.t

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = Api_key.t

  let handle ?(tags = Logs.Tag.empty) api_key =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Disable" ~tags);
    Ok [ Disabled api_key |> Pool_event.api_key ]
  ;;

  let effects = Access.update
end
