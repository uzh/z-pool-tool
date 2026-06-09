module Conformist = Pool_conformist
module User = Pool_user

let src = Logs.Src.create "user_import.cqrs"

module ConfirmImport : sig
  type t = Pool_context.user * User_import.t

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_context.user * User_import.t

  let handle ?(tags = Logs.Tag.empty) (user, user_import) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command ConfirmImport" ~tags);
    let user_import_confirmed =
      User_import.Confirmed user_import |> Pool_event.user_import
    in
    match user with
    | Pool_context.Admin admin ->
      Ok [ Admin.ImportPendingDisabled admin |> Pool_event.admin; user_import_confirmed ]
    | Pool_context.Contact contact ->
      Ok
        [ Contact.ImportPendingDisabled contact |> Pool_event.contact
        ; user_import_confirmed
        ]
    | Pool_context.Guest -> Error Pool_message.(Error.Invalid Field.User)
  ;;
end

module DisableImport : sig
  type t = Pool_context.user * User_import.t

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_context.user * User_import.t

  let handle ?(tags = Logs.Tag.empty) (user, user_import) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command DisableImport" ~tags);
    let user_import_confirmed =
      User_import.Confirmed user_import |> Pool_event.user_import
    in
    match user with
    | Pool_context.Admin admin ->
      Ok [ Admin.ImportPendingDisabled admin |> Pool_event.admin; user_import_confirmed ]
    | Pool_context.Contact contact ->
      Ok
        [ Contact.ImportPendingDisabled contact |> Pool_event.contact
        ; user_import_confirmed
        ]
    | Pool_context.Guest -> Error Pool_message.(Error.Invalid Field.User)
  ;;
end

module ActivateImport : sig
  type t = Pool_context.user * User_import.t

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_context.user * User_import.t

  let handle ?(tags = Logs.Tag.empty) (user, user_import) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command ActivateImport" ~tags);
    let user_import_confirmed =
      User_import.Confirmed user_import |> Pool_event.user_import
    in
    match user with
    | Pool_context.Admin admin ->
      Ok [ Admin.ImportPendingDisabled admin |> Pool_event.admin; user_import_confirmed ]
    | Pool_context.Contact contact ->
      Ok
        [ Contact.TermsAccepted contact |> Pool_event.contact
        ; Contact.ImportPendingDisabled contact |> Pool_event.contact
        ; user_import_confirmed
        ]
    | Pool_context.Guest -> Error Pool_message.(Error.Invalid Field.User)
  ;;
end
