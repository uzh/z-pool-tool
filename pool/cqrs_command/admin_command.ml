open CCFun.Infix
module Conformist = Pool_conformist

let src = Logs.Src.create "admin.cqrs"

module CreateAdmin : sig
  include Common.CommandSig

  type t =
    { email : Pool_user.EmailAddress.t
    ; password : Pool_user.Password.Plain.t [@opaque]
    ; firstname : Pool_user.Firstname.t
    ; lastname : Pool_user.Lastname.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?id:Admin.Id.t
    -> ?roles:(Role.Role.t * Guard.Uuid.Target.t option) list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t =
    { email : Pool_user.EmailAddress.t
    ; password : Pool_user.Password.Plain.t [@opaque]
    ; firstname : Pool_user.Firstname.t
    ; lastname : Pool_user.Lastname.t
    }

  let command email password firstname lastname =
    { email; password; firstname; lastname }
  ;;

  let schema =
    Pool_conformist.(
      make
        Field.
          [ Pool_user.EmailAddress.schema ()
          ; Pool_user.Password.Plain.schema ()
          ; Pool_user.Firstname.schema ()
          ; Pool_user.Lastname.schema ()
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
    { current_password : Pool_user.Password.Plain.t
    ; new_password : Pool_user.Password.Plain.t
    ; password_confirmation : Pool_user.Password.Confirmation.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?notification:Email.job
    -> Admin.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Admin.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { current_password : Pool_user.Password.Plain.t [@opaque]
    ; new_password : Pool_user.Password.Plain.t [@opaque]
    ; password_confirmation : Pool_user.Password.Confirmation.t [@opaque]
    }

  let command current_password new_password password_confirmation =
    { current_password; new_password; password_confirmation }
  ;;

  let schema =
    let open Pool_message.Field in
    Conformist.(
      make
        Field.
          [ Pool_user.Password.Plain.(schema ~field:CurrentPassword ())
          ; Pool_user.Password.Plain.(schema ~field:NewPassword ())
          ; Pool_user.Password.Confirmation.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) ?notification admin command =
    Logs.info ~src (fun m -> m "Handle command UpdatePassword" ~tags);
    (* NOTE use 'Pool_user.validate_current_password' in handler before this
       command. *)
    let open CCResult in
    let* () =
      Pool_user.Password.validate_confirmation
        command.new_password
        command.password_confirmation
    in
    Ok
      ((Pool_user.PasswordUpdated
          ( (admin |> Admin.(id %> Id.to_user))
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.user)
       :: CCOption.map_or
            ~default:[]
            (Email.sent %> Pool_event.email %> CCList.return)
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
      make
        Field.[ Pool_user.Firstname.schema (); Pool_user.Lastname.schema () ]
        command)
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
