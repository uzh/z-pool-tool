module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

module CreateOperator : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) result)
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode : (string * string list) list -> (t, Conformist.error list) result
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
    Pool_common.Utils.PoolConformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.schema "password"
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
      ?allowed_email_suffixes
      ?password_policy
      (_ : Pool_tenant.Write.t)
      command
    =
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.email
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

  let decode data =
    Pool_common.Utils.PoolConformist.decode_and_validate schema data
  ;;
end
