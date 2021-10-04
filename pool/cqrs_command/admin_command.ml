module User = Common_user
module Id = Pool_common.Id

module CreateRoot : sig
  type t =
    { email : User.Email.Address.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { email : User.Email.Address.t
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
          [ User.Email.Address.schema ()
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy command =
    let ( let* ) = Result.bind in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Common_user.Email.Address.validate allowed_email_suffixes command.email
    in
    let admin : Admin.create =
      Admin.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        }
    in
    Ok [ Admin.Created (Admin.Root, admin) |> Pool_event.admin ]
  ;;

  let decode data = Conformist.decode_and_validate schema data

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module ToggleRootStatus : sig
  type t = Admin.root Admin.t

  val handle : Admin.root Admin.t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Admin.root Admin.t

  let handle (root : Admin.root Admin.t) =
    let open Sihl.Contract.User in
    let status = (root |> Admin.user).status in
    match status with
    | Active -> Ok [ Admin.RootDisabled root |> Pool_event.admin ]
    | Inactive -> Ok [ Admin.RootEnabled root |> Pool_event.admin ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module CreateOperator : sig
  type t =
    { email : User.Email.Address.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> Tenant.Write.t
    -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { email : User.Email.Address.t
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
          [ User.Email.Address.schema ()
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
      ?allowed_email_suffixes
      ?password_policy
      command
      (_ : Tenant.Write.t)
    =
    let ( let* ) = Result.bind in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Common_user.Email.Address.validate allowed_email_suffixes command.email
    in
    (* TODO: pass Id or Tenant to Admin.Created function as option to further
       pass down to permissions *)
    let operator : Admin.create =
      Admin.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        }
    in
    Ok [ Admin.Created (Admin.Operator, operator) |> Pool_event.admin ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end
