module User = Pool_user
module Id = Pool_common.Id

module CreateOperator : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; promote : bool
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) result)
    -> (Participant.t, Pool_common.Message.error) result
    -> (Admin.any_person, Pool_common.Message.error) result
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
    ; promote : bool
    }

  let command email password firstname lastname promote =
    { email; password; firstname; lastname; promote }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.schema "password"
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ; Conformist.bool "promote"
          ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy participant admin command =
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
    let bla =
      match[@warning "-4"] participant with
      (* Not a participant => Proceed *)
      | Error Pool_common.Message.(NotFound Participant) ->
        (match admin, promote with
        (* Admin not found, not intending to promote => Create them as an
           operator *)
        | Error Pool_common.Message.(NotFound Admin), false ->
          Ok [ Admin.Created (Admin.Operator, operator) |> Pool_event.admin ]
        (* Something else went wrong => No events *)
        | Error err ->
          Logs.err (fun m ->
              m
                "Could not create operator: %s"
                (Pool_common.Message.show_error err));
          CCResult.fail err
        (* Admin already exists, promote them to operator *)
        (* TODO [aerben] add a confirmation screen*)
        | Ok admin -> PromoteToOperator.handle admin)
      (* Is already a participant => Proceed *)
      | Ok _ ->
        let err = Pool_common.Message.AlreadyParticipant in
        Logs.err (fun m ->
            m
              "Could not create operator: %s"
              (Pool_common.Message.show_error err));
        CCResult.fail err
      (* Something else went wrong => No events *)
      | Error msg ->
        Logs.err (fun m ->
            m
              "Could not create operator: %s"
              (Pool_common.Message.show_error msg));
        CCResult.fail msg
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
    | Any (Operator _) -> Error Pool_common.Message.AlreadyOperator
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
  ;;
end

(* module SetToOperator : sig
 *   val handle
 *     :  (string * string list) list
 *     -> (Participant.t, Pool_common.Message.error) result
 *     -> (Admin.any_person, Pool_common.Message.error) result
 *     -> (Pool_event.t list, Pool_common.Message.error) result
 *
 *   val can : Sihl_user.t -> bool Lwt.t
 * end = struct
 *   let handle urlencoded participant admin =
 *     let open CCResult.Infix in
 *     match[@warning "-4"] participant with
 *     (\* Not a participant => Proceed *\)
 *     | Error Pool_common.Message.(NotFound Participant) ->
 *       (match admin with
 *       (\* Admin not found, create them as an operator *\)
 *       | Error Pool_common.Message.(NotFound Admin) ->
 *         let open CreateOperator in
 *         urlencoded |> decode >>= handle
 *       (\* Something else went wrong => No events *\)
 *       | Error err ->
 *         Logs.err (fun m ->
 *             m
 *               "Could not create operator: %s"
 *               (Pool_common.Message.show_error err));
 *         CCResult.fail err
 *       (\* Admin already exists, promote them to operator *\)
 *       (\* TODO [aerben] add a confirmation screen*\)
 *       | Ok admin -> PromoteToOperator.handle admin)
 *     (\* Is already a participant => Proceed *\)
 *     | Ok _ ->
 *       let err = Pool_common.Message.AlreadyParticipant in
 *       Logs.err (fun m ->
 *           m "Could not create operator: %s" (Pool_common.Message.show_error err));
 *       CCResult.fail err
 *     (\* Something else went wrong => No events *\)
 *     | Error msg ->
 *       Logs.err (fun m ->
 *           m "Could not create operator: %s" (Pool_common.Message.show_error msg));
 *       CCResult.fail msg
 *   ;;
 *
 *   let can user =
 *     Permission.can user ~any_of:[ Permission.Create Permission.Tenant ]
 *   ;;
 * end *)
