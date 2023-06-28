open CCFun
open Utils.Lwt_result.Infix
open Test_utils
module GuardianCommand = Cqrs_command.Guardian_command
module Field = Pool_common.Message.Field

let role_message msg = [%show: Role.Actor.t] %> Format.asprintf msg
let accessible_roles = Handler.Admin.Admin.generate_all_accessible_roles
let check_result = Alcotest.(check (result unit Test_utils.error))

let to_actor database_label admin =
  Admin.id admin
  |> Guard.Uuid.actor_of Admin.Id.value
  |> Guard.Persistence.Actor.find database_label `Admin
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
    `Assistant (Guard.Uuid.target_of Experiment.Id.value id)
    |> Guard.RoleSet.singleton
    |> create_admin database_label firstname
  ;;

  let create_operator database_label =
    Guard.RoleSet.singleton `Operator |> create_admin database_label "Operator"
  ;;
end

let grant_roles _ () =
  let open Guard in
  let db = Test_utils.Data.database_label in
  let%lwt exp1, exp2 =
    Repo.all_experiments () ||> CCList.(fun e -> hd e, nth e 1)
  in
  let operator =
    Data.create_operator db >|> to_actor db >|> accessible_roles db
  in
  let actor =
    Data.create_assistant db "Assistant1" exp1
    >|> to_actor db
    >|> accessible_roles db
  in
  let%lwt target = Data.create_assistant db "Assistant2" exp2 in
  let open GuardianCommand in
  let grant_role role = { target; roles = [ role ] } in
  let target_has_role target role () =
    target |> to_actor db ||> flip Guard.Actor.has_role role
  in
  let handle_validated_events role accessible =
    GrantRoles.validate_role role accessible
    |> Lwt_result.lift
    >== (fun () -> GrantRoles.handle role)
    |>> Pool_event.handle_events db
    >|+ Guard.Persistence.Cache.clear
  in
  let%lwt () =
    let role = `Experimenter Experiment.(exp1.id |> Uuid.target_of Id.value) in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    actor
    >|> handle_validated_events grant
    ||> check_result
          "Grant experimenter rights as assistant fails."
          (Error Pool_common.Message.PermissionDeniedGrantRole)
    >|> target_has_role target role
    ||> Alcotest.(check bool)
          (role_message "Target shouldn't has the granted role (%s)" role)
          false
  in
  let%lwt () =
    let role = `Assistant Experiment.(exp2.id |> Uuid.target_of Id.value) in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    actor
    >|> handle_validated_events grant
    ||> check_result
          "Grant assistant rights as assistant of other experiment fails."
          (Error Pool_common.Message.PermissionDeniedGrantRole)
    >|> target_has_role target role
    ||> Alcotest.(check bool)
          (role_message "Target had permission already (%s)" role)
          true
  in
  let%lwt () =
    let role = `Assistant Experiment.(exp1.id |> Uuid.target_of Id.value) in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    actor
    >|> handle_validated_events grant
    ||> check_result
          "Grant assistant rights as assistant of the experiment works."
          (Ok ())
    >|> target_has_role target role
    ||> Alcotest.(check bool)
          (role_message "Target has the granted role (%s)" role)
          true
  in
  let%lwt () =
    let role = `Assistant Experiment.(exp1.id |> Uuid.target_of Id.value) in
    let ({ target; _ } as grant : grant_role) = grant_role role in
    operator
    >|> handle_validated_events grant
    ||> check_result "Grant experimenter rights as operator works." (Ok ())
    >|> target_has_role target role
    ||> Alcotest.(check bool)
          (role_message "Target has the granted role (%s)" role)
          true
  in
  Lwt.return_unit
;;
