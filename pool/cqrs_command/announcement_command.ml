module Conformist = Pool_conformist
open Announcement

let src = Logs.Src.create "announcement.cqrs"

type command =
  { start_at : StartAt.t option
  ; end_at : EndAt.t option
  ; show_to_admins : ShowToAdmins.t
  ; show_to_contacts : ShowToContacts.t
  }

let command start_at end_at show_to_admins show_to_contacts =
  { start_at; end_at; show_to_admins; show_to_contacts }
;;

let schema =
  Pool_conformist.(
    make
      Field.
        [ Conformist.optional @@ StartAt.schema ()
        ; Conformist.optional @@ EndAt.schema ()
        ; ShowToAdmins.(schema ~default:init ())
        ; ShowToContacts.(schema ~default:init ())
        ]
      command)
;;

let validate_start_end ~start_at ~end_at =
  match start_at, end_at with
  | Some start_at, Some end_at
    when Ptime.is_later (StartAt.value start_at) ~than:(EndAt.value end_at) ->
    Error Pool_message.Error.EndBeforeStart
  | _ -> Ok ()
;;

let validate_display_bools ~show_to_admins ~show_to_contacts =
  match
    ShowToAdmins.value show_to_admins, ShowToContacts.value show_to_contacts
  with
  | false, false ->
    Error
      Pool_message.(
        Error.AtLeastOneSelected (Field.ShowToAdmins, Field.ShowToContacts))
  | _ -> Ok ()
;;

module Create : sig
  include Common.CommandSig

  type t = command

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Announcement.Id.t
    -> (Pool_common.Language.t * string) list
    -> Pool_tenant.Id.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = command

  let handle
    ?(tags = Logs.Tag.empty)
    ?id
    text
    tenant_ids
    ({ start_at; end_at; show_to_admins; show_to_contacts } : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let* () = validate_start_end ~start_at ~end_at in
    let* () = validate_display_bools ~show_to_admins ~show_to_contacts in
    let* text = Text.create text in
    let announcement =
      create ?id text start_at end_at show_to_admins show_to_contacts
    in
    Ok [ Created (announcement, tenant_ids) |> Pool_event.announcement ]
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
    -> Announcement.t
    -> (Pool_common.Language.t * string) list
    -> Pool_tenant.Id.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = command

  let handle
    ?(tags = Logs.Tag.empty)
    announcement
    text
    tenant_ids
    ({ start_at; end_at; show_to_admins; show_to_contacts } : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let* () = validate_start_end ~start_at ~end_at in
    let* () = validate_display_bools ~show_to_admins ~show_to_contacts in
    let* text = Text.create text in
    let updated =
      { announcement with
        text
      ; start_at
      ; end_at
      ; show_to_admins
      ; show_to_contacts
      }
    in
    Ok [ Updated (updated, tenant_ids) |> Pool_event.announcement ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.update
end