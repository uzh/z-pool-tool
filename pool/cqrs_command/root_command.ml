module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "root.cqrs"

module Create : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:
         (User.Password.t -> (unit, Pool_common.Message.error) result)
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  let command email password firstname lastname =
    { email; password; firstname; lastname }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    ?password_policy
    command
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.email
    in
    let admin : Admin.create =
      Admin.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        }
    in
    Ok [ Admin.Created admin |> Pool_event.admin ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Manage, `TargetEntity `System ]
end

module ToggleStatus : sig
  type t = Admin.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t = Admin.t

  let handle ?(tags = Logs.Tag.empty) (admin : Admin.t) =
    Logs.info ~src (fun m -> m "Handle command ToggleStatus" ~tags);
    let open Sihl.Contract.User in
    let status = (admin |> Admin.user).status in
    match status with
    | Active -> Ok [ Admin.Disabled admin |> Pool_event.admin ]
    | Inactive -> Ok [ Admin.Enabled admin |> Pool_event.admin ]
  ;;

  let effects = [ `Manage, `TargetEntity `System ]
end
