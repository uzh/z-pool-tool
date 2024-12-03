module Conformist = Pool_conformist
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "root.cqrs"

module Create : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.Plain.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Guard.ValidationSet.t
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.Plain.t [@opaque]
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  let command email password firstname lastname = { email; password; firstname; lastname }

  let schema =
    Conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.Plain.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) ?allowed_email_suffixes command =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* () = Pool_user.EmailAddress.validate allowed_email_suffixes command.email in
    let admin : Admin.create =
      { id = None
      ; Admin.email = command.email
      ; password = command.password
      ; firstname = command.firstname
      ; lastname = command.lastname
      ; roles = [ `Operator, None ]
      }
    in
    Ok [ Admin.Created admin |> Pool_event.admin ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects =
    let open Guard in
    ValidationSet.one_of_tuple (Permission.Manage, `System, None)
  ;;
end

module ToggleStatus : sig
  include Common.CommandSig

  type t = Admin.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Admin.t

  let handle ?(tags = Logs.Tag.empty) (admin : Admin.t) =
    Logs.info ~src (fun m -> m "Handle command ToggleStatus" ~tags);
    let open Pool_user.Status in
    match (admin |> Admin.user).Pool_user.status with
    | Active -> Ok [ Admin.Disabled admin |> Pool_event.admin ]
    | Inactive -> Ok [ Admin.Enabled admin |> Pool_event.admin ]
  ;;

  let effects =
    let open Guard in
    ValidationSet.(one_of_tuple (Permission.Manage, `Admin, None))
  ;;
end
