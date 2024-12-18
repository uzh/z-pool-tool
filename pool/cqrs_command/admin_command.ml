let src = Logs.Src.create "admin.cqrs"

type grant_role =
  { target_id : Guard.Uuid.Actor.t
  ; roles : (Role.Role.t * Guard.Uuid.Target.t option) list
  }

type revoke_role =
  { target_id : Guard.Uuid.Actor.t
  ; role : Role.Role.t * Guard.Uuid.Target.t option
  }

module CreateAdmin : sig
  include Common.CommandSig

  type t = User_command.create_user

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?id:Admin.Id.t
    -> ?roles:(Role.Role.t * Guard.Uuid.Target.t option) list
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t = User_command.create_user

  let handle ?(tags = Logs.Tag.empty) ?allowed_email_suffixes ?id ?(roles = []) command =
    Logs.info ~src (fun m -> m "Handle command CreateAdmin" ~tags);
    let open CCResult in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.User_command.email
    in
    (* TODO: pass Id or Tenant to Admin.Created function as option to further pass down to
       permissions *)
    let admin : Admin.create =
      User_command.
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
    Pool_conformist.decode_and_validate User_command.create_user_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Admin.Guard.Access.create
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
      make Field.[ Pool_user.Firstname.schema (); Pool_user.Lastname.schema () ] command)
  ;;

  let handle ?(tags = Logs.Tag.empty) admin update =
    Logs.info ~src (fun m -> m "Handle command CreateAdmin" ~tags);
    Ok [ Admin.DetailsUpdated (admin, update) |> Pool_event.admin ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Admin.Guard.Access.create
end

module UpdateSignInCount : sig
  type t = Admin.t

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
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

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
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
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects =
    let open Guard in
    let open ValidationSet in
    Or [ one_of_tuple (Permission.Create, `RoleAdmin, None); Admin.Guard.Access.create ]
  ;;
end
