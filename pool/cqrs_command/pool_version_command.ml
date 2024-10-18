module Conformist = Pool_conformist
open Pool_version

let src = Logs.Src.create "pool_version.cqrs"

type command =
  { version : Version.t
  ; text : Text.t
  }

let command version text = { version; text }

let create_schema =
  Pool_conformist.(make Field.[ Version.schema (); Text.schema () ] command)
;;

let update_schema = Pool_conformist.(make Field.[ Text.schema () ] CCFun.id)

module Create : sig
  include Common.CommandSig

  type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Pool_version.Id.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = command

  let handle ?(tags = Logs.Tag.empty) ?id ({ version; text } : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let version = create ?id version text in
    Ok [ Created version |> Pool_event.pool_version ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate create_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.create
end

module Update : sig
  include Common.CommandSig

  type t = Text.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_version.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = Text.t

  let handle ?(tags = Logs.Tag.empty) (version : Pool_version.t) text =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let updated = { version with text } in
    Ok [ Updated updated |> Pool_event.pool_version ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.update
end
