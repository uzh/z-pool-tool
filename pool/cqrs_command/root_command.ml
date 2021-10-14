module User = Common_user
module Id = Pool_common.Id

module Create : sig
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
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Common_user.Email.Address.validate allowed_email_suffixes command.email
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

  let decode data = Conformist.decode_and_validate schema data

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module ToggleStatus : sig
  type t = Root.t

  val handle : Root.t -> (Pool_event.t list, string) Result.t
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
