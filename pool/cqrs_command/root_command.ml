module User = Pool_user
module Id = Pool_common.Id

module Create : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) result)
    -> t
    -> (Pool_event.t list, Pool_common.Message.t) result

  val decode : (string * string list) list -> (t, Pool_common.Message.t) result
  val can : Sihl_user.t -> t -> bool Lwt.t
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
          ; User.Password.schema "password"
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy command =
    let open CCResult in
    let* () =
      User.Password.validate ?password_policy command.password
      |> map_err Pool_common.Message.errorm
    in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.email
      |> map_err Pool_common.Message.errorm
    in
    let admin : Root.create =
      Root.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        }
    in
    Ok [ Root.Created admin |> Pool_event.root ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err (fun e -> Pool_common.Message.(ErrorM (Conformist e)))
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module ToggleStatus : sig
  type t = Root.t

  val handle : Root.t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Root.t

  let handle (root : Root.t) =
    let open Sihl.Contract.User in
    let status = (root |> Root.user).status in
    match status with
    | Active -> Ok [ Root.Disabled root |> Pool_event.root ]
    | Inactive -> Ok [ Root.Enabled root |> Pool_event.root ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
