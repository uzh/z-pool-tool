open CCFun
open Utils.Lwt_result.Infix
open Test_utils
module GuardianCommand = Cqrs_command.Guardian_command
module Field = Pool_common.Message.Field

let role_message msg (role, uuid) =
  [%show: Role.Role.t] role
  :: (uuid
      |> CCOption.map_or
           ~default:[]
           ([%show: Guard.Uuid.Target.t]
            %> Format.asprintf "(%s)"
            %> CCList.return))
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
    |>> Pool_event.handle_events database_label
    >>= (fun () -> Admin.find database_label id)
    ||> Pool_common.Utils.get_or_failwith
  ;;

  let create_assistant database_label firstname { Experiment.id; _ } =
    [ `Assistant, Some (Guard.Uuid.target_of Experiment.Id.value id) ]
    |> create_admin database_label firstname
  ;;

  let create_operator database_label =
    [ `Operator, None ] |> create_admin database_label "Operator"
  ;;
end

let grant_roles _ () =
  let open Guard in
  let db = Test_utils.Data.database_label in
  let%lwt exp1, exp2 =
    Repo.all_experiments () ||> CCList.(fun e -> hd e, nth e 1)
  in
  let%lwt operator = Data.create_operator db >|> to_actor db in
  let%lwt actor = Data.create_assistant db "Assistant1" exp1 >|> to_actor db in
  let%lwt target = Data.create_assistant db "Assistant2" exp2 in
  let target_has_role target role () =
    Guard.(
      Persistence.ActorRole.find_by_actor
        (Uuid.actor_of Admin.Id.value (Admin.id target)))
    ||> CCList.exists (ActorRole.equal role)
  in
  let open GuardianCommand in
  let grant_role role = { target; roles = [ role ] } in
  let handle_validated_events role =
    GrantRoles.handle role
    |> Lwt_result.lift
    |>> Pool_event.handle_events db
    >|+ Guard.Persistence.Cache.clear
  in
  let%lwt () =
    let ((target_role, target_uuid) as role) =
      `Experimenter, Some Experiment.(exp1.id |> Uuid.target_of Id.value)
    in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    handle_validated_events grant
    ||> check_result
          "Grant experimenter rights as assistant fails."
          (Error Pool_common.Message.PermissionDeniedGrantRole)
    >|> target_has_role
          target
          (ActorRole.create ?target_uuid actor.Actor.uuid target_role)
    ||> Alcotest.(check bool)
          (role_message "Target shouldn't has the granted role (%s)" role)
          false
  in
  let%lwt () =
    let ((target_role, target_uuid) as role) =
      `Assistant, Some Experiment.(exp2.id |> Uuid.target_of Id.value)
    in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    handle_validated_events grant
    ||> check_result
          "Grant assistant rights as assistant of other experiment fails."
          (Error Pool_common.Message.PermissionDeniedGrantRole)
    >|> target_has_role
          target
          (ActorRole.create ?target_uuid actor.Actor.uuid target_role)
    ||> Alcotest.(check bool)
          (role_message "Target had permission already (%s)" role)
          true
  in
  let%lwt () =
    let ((target_role, target_uuid) as role) =
      `Assistant, Some Experiment.(exp1.id |> Uuid.target_of Id.value)
    in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    handle_validated_events grant
    ||> check_result
          "Grant assistant rights as assistant of the experiment works."
          (Ok ())
    >|> target_has_role
          target
          (ActorRole.create ?target_uuid actor.Actor.uuid target_role)
    ||> Alcotest.(check bool)
          (role_message "Target has the granted role (%s)" role)
          true
  in
  let%lwt () =
    let ((target_role, target_uuid) as role) =
      `Assistant, Some Experiment.(exp1.id |> Uuid.target_of Id.value)
    in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    handle_validated_events grant
    ||> check_result "Grant experimenter rights as operator works." (Ok ())
    >|> target_has_role
          target
          (ActorRole.create ?target_uuid operator.Actor.uuid target_role)
    ||> Alcotest.(check bool)
          (role_message "Target has the granted role (%s)" role)
          true
  in
  Lwt.return_unit
;;
