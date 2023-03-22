module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "admin.cqrs"

module CreateAdmin : sig
  include Common.CommandSig

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
    -> ?id:Pool_common.Id.t
    -> ?roles:Guard.RoleSet.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
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
    Pool_common.Utils.PoolConformist.(
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
    ?id
    ?roles
    command
    =
    Logs.info ~src (fun m -> m "Handle command CreateAdmin" ~tags);
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
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
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects =
    let open Guard in
    ValidationSet.One (Action.Create, TargetSpec.Entity `Admin)
  ;;
end
