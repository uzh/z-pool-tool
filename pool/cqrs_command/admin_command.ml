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
    -> (Participant.t, Pool_common.Message.error) result
    -> (Admin.any_person, Pool_common.Message.error) result
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

  let handle ?allowed_email_suffixes ?password_policy participant admin command =
    let open CCResult in
    let* () =
      User.Password.validate ?password_policy command.password
      |> map_err Pool_common.Message.errorm
    in
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes command.email
      |> map_err Pool_common.Message.errorm
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
    let handle_msg msg =
      Logs.err (fun m ->
          m "Could not create operator: %s" (Pool_common.Message.show msg));
      CCResult.fail msg
    in
    Pool_common.(
      match[@warning "-4"] admin, participant with
      (* Not an admin nor a participant => Create *)
      | Message.(Error (NotFound Admin), Error (NotFound Participant)) ->
        Ok [ Admin.Created (Admin.Operator, operator) |> Pool_event.admin ]
      (* Exists as admin, not as participant => No events with hint to
         promote *)
      | Ok _, Error Message.(NotFound Participant) ->
        handle_msg
          Message.(
            MessageList
              [ ErrorM (AlreadyExists Admin); InfoM TryPromoteOperator ])
      (* Exists as participant, not as admin => No events *)
      | Error Message.(NotFound Admin), Ok _ ->
        handle_msg Message.(ErrorM (AlreadyExists Participant))
      (* Something else went wrong => No events *)
      | Error msg, _ | _, Error msg -> handle_msg Message.(ErrorM msg)
      (* User is admin and participant, should not happen => No events *)
      | Ok _, Ok _ -> handle_msg Message.(ErrorM AdminAndParticipantSimul))
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err (fun e -> Pool_common.Message.(ErrorM (Conformist e)))
  ;;
end

module PromoteToOperator : sig
  val handle
    :  Admin.any_person
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> bool Lwt.t
end = struct
  let handle admin =
    let open Admin in
    let event ctor p =
      Ok [ ctor (RoleUpdated (p, Operator)) |> Pool_event.admin ]
    in
    match admin with
    | Any (Assistant _ as p) -> event (fun m -> AssistantEvents m) p
    | Any (Experimenter _ as p) -> event (fun m -> ExperimenterEvents m) p
    | Any (LocationManager _ as p) -> event (fun m -> LocationManagerEvents m) p
    | Any (Recruiter _ as p) -> event (fun m -> RecruiterEvents m) p
    | Any (Operator _) -> Error Pool_common.Message.(AlreadyExists Operator)
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end
