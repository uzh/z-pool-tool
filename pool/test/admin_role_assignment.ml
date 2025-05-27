open CCFun
open Utils.Lwt_result.Infix
open Test_utils
open Pool_message
module AdminCommand = Cqrs_command.Admin_command
module GuardianCommand = Cqrs_command.Guardian_command

let current_user = Model.create_admin ()

let role_message msg (role, uuid) =
  [%show: Role.Role.t] role
  :: (uuid
      |> CCOption.map_or
           ~default:[]
           ([%show: Guard.Uuid.Target.t] %> Format.asprintf "(%s)" %> CCList.return))
  |> CCString.concat " "
  |> Format.asprintf msg
;;

let check_result = Alcotest.(check (result unit Test_utils.error))

let to_actor database_label admin =
  Admin.id admin
  |> Guard.Uuid.actor_of Admin.Id.value
  |> Guard.Persistence.Actor.find database_label
  ||> CCResult.get_or_failwith
;;

module Data = struct
  let create_admin database_label firstname roles =
    let open Cqrs_command.Admin_command.CreateAdmin in
    let id = Admin.Id.create () in
    let email =
      Format.asprintf "admin+%s@mail.com" (CCString.lowercase_ascii firstname)
    in
    [ Field.Email, email
    ; Field.Firstname, firstname
    ; Field.Lastname, "TheAdmin"
    ; Field.Password, "somePassword1!"
    ]
    |> CCList.map (fun (key, value) -> Field.show key, [ value ])
    |> Lwt.return
    ||> decode
    >== handle ~id ~roles
    |>> Pool_event.handle_events database_label current_user
    >>= (fun () -> Admin.find database_label id)
    ||> Pool_common.Utils.get_or_failwith
  ;;

  let create_assistant database_label firstname { Experiment.id; _ } =
    [ `Assistant, Some (Guard.Uuid.target_of Experiment.Id.value id) ]
    |> create_admin database_label firstname
  ;;

  let create_operator ?(firstname = "Operator") database_label =
    [ `Operator, None ] |> create_admin database_label firstname
  ;;

  let create_recruiter ?(firstname = "Recruiter") database_label =
    [ `Recruiter, None ] |> create_admin database_label firstname
  ;;
end

let target_has_role db target (target_role, target_uuid) () =
  let open Guard in
  let actor = Uuid.actor_of Admin.Id.value (Admin.id target) in
  let actor_role = ActorRole.create ?target_uuid actor target_role in
  let%lwt actor_roles =
    Persistence.ActorRole.find_by_actor db actor ||> CCList.map (fun (role, _, _) -> role)
  in
  actor_roles |> CCList.mem ~eq:ActorRole.equal actor_role |> Lwt.return
;;

let handle_validated_events db admin actor role =
  let open GuardianCommand in
  let grant_role role =
    { target_id = Guard.Uuid.actor_of Admin.Id.value (Admin.id admin); roles = [ role ] }
  in
  let validate = Guard.Persistence.Actor.validate_assign_role db in
  validate actor role
  >|+ grant_role
  >== GrantRoles.handle
  |>> Pool_event.handle_events db current_user
  >|+ Guard.Persistence.Cache.clear
;;

let handle_create_role_permission_events db role_permission =
  GuardianCommand.CreateRolePermission.handle role_permission
  |> Lwt_result.lift
  |>> Pool_event.handle_events db current_user
;;

let handle_delete_role_permission_events db role_permission =
  GuardianCommand.DeleteRolePermission.handle role_permission
  |> Lwt_result.lift
  |>> Pool_event.handle_events db current_user
;;

let assignable_roles _ () =
  let open Guard in
  let db = Test_utils.Data.database_label in
  let%lwt exp1, exp2 = Experiment.all db ||> CCList.(fun e -> hd e, nth e 1) in
  let%lwt recruiter =
    Data.create_recruiter ~firstname:"RecruiterAssignable" db >|> to_actor db
  in
  let%lwt targetOne = Data.create_assistant db "AssistantAssignableOne" exp2 in
  let%lwt targetTwo = Data.create_assistant db "AssistantAssignableTwo" exp2 in
  let new_role = `Experimenter in
  let assignable_role =
    RolePermission.create `Recruiter Permission.Create `RoleExperimenter
  in
  let validate_role_assignment target should_success =
    let role = new_role, Some Experiment.(exp1.id |> Uuid.target_of Id.value) in
    let expected, (msg : (string -> 'a, Format.formatter, unit, string) format4) =
      if should_success
      then Ok (), "Target has the granted role (%s)"
      else
        ( Error Error.PermissionDeniedGrantRole
        , "Target shouldn't has the granted role (%s)" )
    in
    handle_validated_events db target recruiter role
    ||> check_result "Grant experimenter rights as recruiter" expected
    >|> target_has_role db target role
    ||> Alcotest.(check bool) (role_message msg role) should_success
  in
  (* As the assignable role isn't created yet, the first validation should fail *)
  let%lwt () = validate_role_assignment targetOne false in
  (* Add the role assignment to the database *)
  let%lwt () =
    assignable_role |> handle_create_role_permission_events db ||> get_or_failwith
  in
  (* The reevaluation of the role assignment should work now *)
  let%lwt () = validate_role_assignment targetOne true in
  (* Remove the assignable role *)
  let%lwt () =
    assignable_role |> handle_delete_role_permission_events db ||> get_or_failwith
  in
  (* As the assignable role is deleted, assignment shouldn't be possible *)
  let%lwt () = validate_role_assignment targetTwo false in
  Lwt.return_unit
;;

let grant_roles _ () =
  let open Guard in
  let db = Test_utils.Data.database_label in
  let%lwt exp1, exp2 = Experiment.all db ||> CCList.(fun e -> hd e, nth e 1) in
  let%lwt operator = Data.create_operator db >|> to_actor db in
  let%lwt actor = Data.create_assistant db "Assistant1" exp1 >|> to_actor db in
  let%lwt target = Data.create_assistant db "Assistant2" exp2 in
  let%lwt () =
    [ `Operator, `RoleAssistant; `Operator, `RoleExperimenter ]
    |> CCList.map (fun (role, target) ->
      RolePermission.create role Permission.Create target)
    |> Lwt_list.iter_s (fun m ->
      Persistence.RolePermission.insert db m ||> CCResult.get_or_failwith)
  in
  let target_has_role = target_has_role db target in
  let handle_validated_events = handle_validated_events db target in
  let%lwt () =
    let role = `Experimenter, Some Experiment.(exp1.id |> Uuid.target_of Id.value) in
    handle_validated_events actor role
    ||> check_result
          "Grant experimenter rights as assistant fails."
          (Error Error.PermissionDeniedGrantRole)
    >|> target_has_role role
    ||> Alcotest.(check bool)
          (role_message "Target shouldn't has the granted role (%s)" role)
          false
  in
  let%lwt () =
    let role = `Assistant, Some Experiment.(exp2.id |> Uuid.target_of Id.value) in
    handle_validated_events actor role
    ||> check_result
          "Grant assistant rights as assistant of other experiment fails."
          (Error Error.PermissionDeniedGrantRole)
    >|> target_has_role role
    ||> Alcotest.(check bool)
          (role_message "Target had permission already (%s)" role)
          true
  in
  let%lwt () =
    let role = `Assistant, Some Experiment.(exp1.id |> Uuid.target_of Id.value) in
    handle_validated_events actor role
    ||> check_result
          "Grant assistant rights as assistant of the experiment fails."
          (Error Error.PermissionDeniedGrantRole)
    >|> target_has_role role
    ||> Alcotest.(check bool) (role_message "Target has the granted role (%s)" role) false
  in
  let%lwt () =
    let role = `Experimenter, Some Experiment.(exp1.id |> Uuid.target_of Id.value) in
    handle_validated_events operator role
    ||> check_result "Grant experimenter rights as operator works." (Ok ())
    >|> target_has_role role
    ||> Alcotest.(check bool) (role_message "Target has the granted role (%s)" role) true
  in
  Lwt.return_unit
;;
