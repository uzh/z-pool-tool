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

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

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

  let handle
      ?allowed_email_suffixes
      ?password_policy
      (_ : Pool_tenant.Write.t)
      command
    =
    let open CCResult in
    (* TODO [aerben] why this not in schema decoding? *)
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
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;
end

(* TODO [aerben] maybe can use this *)
(* module type Command = sig
 *   type t
 *
 *   val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
 *
 *   val decode
 *     :  (string * string list) list
 *     -> (t, Pool_common.Message.error) result
 *
 *   val can : Sihl_user.t -> t -> bool Lwt.t
 * end *)

module PromoteToOperator : sig
  val handle
    :  Admin.any_person
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> bool Lwt.t
end = struct
  let handle person =
    let open Admin in
    let event ctor p =
      Ok [ ctor (RoleUpdated (p, Operator)) |> Pool_event.admin ]
    in
    match person with
    | Any (Assistant _ as p) -> event (fun m -> AssistantEvents m) p
    | Any (Experimenter _ as p) -> event (fun m -> ExperimenterEvents m) p
    | Any (LocationManager _ as p) -> event (fun m -> LocationManagerEvents m) p
    | Any (Recruiter _ as p) -> event (fun m -> RecruiterEvents m) p
    | Any (Operator _) -> Error Pool_common.Message.AlreadyOperator
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end
