module Conformist = Pool_conformist
module User = Pool_user

let src = Logs.Src.create "user_import.cqrs"

module ConfirmImport : sig
  type t =
    { password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> User_import.t * Pool_context.user
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t =
    { password : User.Password.t [@opaque]
    ; password_confirmation : User.PasswordConfirmed.t [@opaque]
    }

  let command password password_confirmation =
    { password; password_confirmation }
  ;;

  let schema =
    let open Pool_message.Field in
    Conformist.(
      make
        Field.
          [ User.Password.(schema ~field:Password create ())
          ; User.PasswordConfirmed.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (user_import, user) command =
    let open Pool_context in
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command ConfirmImport" ~tags);
    let* () =
      User.Password.validate_password_confirmation
        command.password
        command.password_confirmation
    in
    let user_import_confirmed =
      User_import.Confirmed user_import |> Pool_event.user_import
    in
    match user with
    | Admin admin ->
      Ok
        [ Admin.ImportConfirmed (admin, command.password) |> Pool_event.admin
        ; user_import_confirmed
        ]
    | Contact contact ->
      Ok
        [ Contact.ImportConfirmed (contact, command.password)
          |> Pool_event.contact
        ; user_import_confirmed
        ]
    | Guest -> Error Pool_message.(Error.Invalid Field.User)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
end

module DisableImport : sig
  type t = Pool_context.user * User_import.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_context.user * User_import.t

  let handle ?(tags = Logs.Tag.empty) (user, user_import) =
    let open Pool_context in
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command DisableImport" ~tags);
    let user_import_confirmed =
      User_import.Confirmed user_import |> Pool_event.user_import
    in
    match user with
    | Admin admin ->
      Ok
        [ Admin.ImportDisabled admin |> Pool_event.admin
        ; user_import_confirmed
        ]
    | Contact contact ->
      Ok
        [ Contact.ImportDisabled contact |> Pool_event.contact
        ; user_import_confirmed
        ]
    | Guest -> Error Pool_message.(Error.Invalid Field.User)
  ;;
end
