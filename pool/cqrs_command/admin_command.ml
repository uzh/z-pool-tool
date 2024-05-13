let src = Logs.Src.create "admin.cqrs"

type grant_role =
  { target : Admin.t
  ; roles : (Role.Role.t * Guard.Uuid.Target.t option) list
  }

type revoke_role =
  { target : Admin.t
  ; role : Role.Role.t * Guard.Uuid.Target.t option
  }

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
    Pool_conformist.decode_and_validate schema data
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
      make
        Field.[ Pool_user.Firstname.schema (); Pool_user.Lastname.schema () ]
        command)
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
    Pool_conformist.decode_and_validate schema data
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

module GrantRoles : sig
  include Common.CommandSig with type t = grant_role
end = struct
  open Guard

  type t = grant_role

  let handle ?(tags = Logs.Tag.empty) { target; roles } =
    Logs.info ~src (fun m -> m "Handle command GrantRoles" ~tags);
    let actor_roles =
      let to_id admin =
        admin |> Admin.id |> Guard.Uuid.actor_of Admin.Id.value
      in
      CCList.map
        (fun (role, target_uuid) ->
          Guard.ActorRole.create ?target_uuid (target |> to_id) role)
        roles
    in
    Ok
      [ Guard.RolesGranted actor_roles |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects = Guard.Access.Role.create
end

module RevokeRole : sig
  include Common.CommandSig with type t = revoke_role
end = struct
  open Guard

  type t = revoke_role

  let handle ?(tags = Logs.Tag.empty) { target; role } =
    Logs.info ~src (fun m -> m ~tags "Handle command RevokeRole");
    let actor_roles =
      let role, target_uuid = role in
      let to_id admin =
        admin |> Admin.id |> Guard.Uuid.actor_of Admin.Id.value
      in
      Guard.ActorRole.create ?target_uuid (target |> to_id) role
    in
    Ok
      [ Guard.RolesRevoked [ actor_roles ] |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects = Guard.Access.Role.delete
end
