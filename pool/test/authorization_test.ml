let update_language_as actor =
  let open Utils.Lwt_result.Infix in
  let subject =
    "john@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let* tenant = Pool_tenant.find_by_label Test_utils.Data.database_label in
  let ctx = Database.to_ctx Test_utils.Data.database_label in
  let effects =
    Cqrs_command.Contact_command.Update.effects (Contact.id subject)
  in
  let* (_ : Guard.Target.t) =
    Pool_tenant.Guard.Target.to_authorizable ~ctx tenant
  in
  let* (_ : Guard.Target.t) =
    Contact.Guard.Target.to_authorizable ~ctx subject
  in
  let* (_ : Guard.Actor.t) =
    Pool_tenant.Guard.Actor.to_authorizable ~ctx tenant
  in
  let* (_ : Guard.Actor.t) = Contact.Guard.Actor.to_authorizable ~ctx subject in
  let* () =
    Guard.Persistence.validate Test_utils.Data.database_label effects actor
  in
  Lwt.return_ok ()
;;

let recruiter_can_update_contact_language _ () =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx Test_utils.Data.database_label in
  let%lwt actor =
    let open Guard.Persistence in
    ActorRole.find_actors_by_role ~ctx (`Recruiter, None)
    ||> CCList.hd
    >|> Actor.find Test_utils.Data.database_label
    ||> CCResult.get_or_failwith
  in
  let%lwt actual = update_language_as actor in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a contact."
    (Ok ())
    actual
  |> Lwt.return
;;

let guest_cannot_update_language _ () =
  let%lwt actual = update_language_as Guard.guest_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Guest cannot update a contact."
    (Error (Pool_message.Error.authorization "Failure"))
    (CCResult.map_err
       (fun _ -> Pool_message.Error.authorization "Failure")
       actual)
  |> Lwt.return
;;

let operator_works _ () =
  let ctx = Database.to_ctx Test_utils.Data.database_label in
  let%lwt actual =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let to_error = Pool_message.Error.authorization in
    let target =
      "chris@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* _ = Contact.Guard.Actor.to_authorizable ~ctx target in
    let subject =
      "john@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* actor = Contact.Guard.Actor.to_authorizable ~ctx subject in
    let%lwt () =
      ActorRole.create actor.Actor.uuid `Operator
      |> Persistence.ActorRole.upsert ~ctx
      ||> CCFun.tap (fun () -> Persistence.Cache.clear ())
    in
    let* actor = Contact.Guard.Actor.to_authorizable ~ctx subject in
    let* () =
      let open Guard in
      RolePermission.create `Operator Permission.Manage `Contact
      |> Persistence.RolePermission.insert Test_utils.Data.database_label
      >|- to_error
      ||> CCFun.tap (fun _ -> Persistence.Cache.clear ())
    in
    let effects =
      ValidationSet.one_of_tuple (Permission.Manage, `Contact, None)
    in
    let* () =
      Persistence.validate Test_utils.Data.database_label effects actor
    in
    Lwt_result.return ()
  in
  Alcotest.(check (result unit Test_utils.error))
    "Parametric roles work."
    (Ok ())
    actual
  |> Lwt.return
;;
