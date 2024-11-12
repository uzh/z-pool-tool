module Conformist = Pool_conformist
open Pool_version

let src = Logs.Src.create "pool_version.cqrs"

type command =
  { tag : Tag.t
  ; text : Text.t
  }

let command tag text = { tag; text }

let create_schema =
  Pool_conformist.(make Field.[ Tag.schema (); Text.schema () ] command)
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

  let handle ?(tags = Logs.Tag.empty) ?id ({ tag; text } : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let version = create ?id tag text in
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

module Publish : sig
  type t = Pool_version.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?announcement_id:Announcement.Id.t
    -> Pool_tenant.Id.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_version.t

  let handle ?(tags = Logs.Tag.empty) ?announcement_id tenant_ids version =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Publish" ~tags);
    let* () =
      if Option.is_some version.published_at
      then Error Pool_message.(Error.AlreadyPublished Field.Version)
      else Ok ()
    in
    let* announcemet = announcement ?id:announcement_id version in
    Ok
      Pool_event.
        [ Published version |> pool_version
        ; Announcement.Created (announcemet, tenant_ids) |> announcement
        ]
  ;;

  let effects = Access.update
end
