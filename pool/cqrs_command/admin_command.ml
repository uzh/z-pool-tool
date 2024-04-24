module Conformist = Pool_conformist
module User = Pool_user

let src = Logs.Src.create "admin.cqrs"

module CreateAdmin : sig
  include Common.CommandSig

  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t [@opaque]
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?id:Pool_user.Id.t
    -> ?roles:(Role.Role.t * Guard.Uuid.Target.t option) list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t [@opaque]
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  let command email password firstname lastname =
    { email; password; firstname; lastname }
  ;;

  let schema =
    Pool_conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.(schema create ())
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    ?id
    ?(roles = [])
    command
    =
    Logs.info ~src (fun m -> m "Handle command CreateAdmin" ~tags);
    let open CCResult in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.email
    in
    (* TODO: pass Id or Tenant to Admin.Created function as option to further
       pass down to permissions *)
    let admin : Admin.create =
      { id
      ; Admin.email = command.email
      ; password = command.password
      ; firstname = command.firstname
      ; lastname = command.lastname
      ; roles
      }
    in
    Ok [ Admin.Created admin |> Pool_event.admin ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Admin.Guard.Access.create
end

module UpdatePassword : sig
  include Common.CommandSig

  type t =
    { current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?notification:Email.job
    -> Admin.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Pool_user.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { current_password : User.Password.t [@opaque]
    ; new_password : User.Password.t [@opaque]
    ; password_confirmation : User.PasswordConfirmed.t [@opaque]
    }

  let command current_password new_password password_confirmation =
    { current_password; new_password; password_confirmation }
  ;;

  let schema =
    let open Pool_message.Field in
    Conformist.(
      make
        Field.
          [ User.Password.(schema ~field:CurrentPassword create_unvalidated ())
          ; User.Password.(schema ~field:NewPassword create ())
          ; User.PasswordConfirmed.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) ?notification admin command =
    Logs.info ~src (fun m -> m "Handle command UpdatePassword" ~tags);
    let open CCResult in
    let* () =
      User.validate_current_password (Admin.user admin) command.current_password
    in
    let* () =
      User.Password.validate_password_confirmation
        command.new_password
        command.password_confirmation
    in
    Ok
      ((Admin.PasswordUpdated
          ( admin
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.admin)
       :: CCOption.map_or
            ~default:[]
            (fun note -> Email.Sent note |> Pool_event.email |> CCList.return)
            notification)
  ;;

  let effects = Admin.Guard.Access.update

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
end

module Update : sig
  include Common.CommandSig

  type t = Admin.update

  val handle
    :  ?tags:Logs.Tag.set
    -> Admin.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = Admin.update

  let command firstname lastname = { Admin.firstname; lastname }

  let schema =
    Pool_conformist.(
      make Field.[ User.Firstname.schema (); User.Lastname.schema () ] command)
  ;;

  let handle ?(tags = Logs.Tag.empty) admin update =
    Logs.info ~src (fun m -> m "Handle command CreateAdmin" ~tags);
    Ok [ Admin.DetailsUpdated (admin, update) |> Pool_event.admin ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Admin.Guard.Access.create
end

module UpdateSignInCount : sig
  type t = Admin.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Admin.t

  let handle ?(tags = Logs.Tag.empty) admin =
    Logs.info ~src (fun m -> m "Handle command UpdateSignInCount" ~tags);
    Ok [ Admin.SignInCounterUpdated admin |> Pool_event.admin ]
  ;;
end

module PromoteContact : sig
  include Common.CommandSig

  type t = Pool_user.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = Pool_user.Id.t

  let schema =
    let open Pool_conformist in
    make Field.[ Pool_user.Id.schema () ] CCFun.id
  ;;

  let handle ?(tags = Logs.Tag.empty) id =
    Logs.info ~src (fun m -> m "Handle command PromoteContact" ~tags);
    Ok [ Admin.PromotedContact id |> Pool_event.admin ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects =
    let open Guard in
    let open ValidationSet in
    Or
      [ one_of_tuple (Permission.Create, `RoleAdmin, None)
      ; Admin.Guard.Access.create
      ]
  ;;
end
