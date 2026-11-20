let update_language_as db_ctx actor =
  let open Utils.Lwt_result.Infix in
  let subject =
    "john@gmail.com" |> Contact_test.contact_info |> Contact_test.create_contact true
  in
  let* tenant = Pool_tenant.find_by_db_ctx db_ctx in
  let ctx = Database.to_ctx db_ctx in
  let effects = Cqrs_command.Contact_command.Update.effects (Contact.id subject) in
  let* (_ : Guard.Target.t) = Pool_tenant.Guard.Target.to_authorizable ~ctx tenant in
  let* (_ : Guard.Target.t) = Contact.Guard.Target.to_authorizable ~ctx subject in
  let* (_ : Guard.Actor.t) = Pool_tenant.Guard.Actor.to_authorizable ~ctx tenant in
  let* (_ : Guard.Actor.t) = Contact.Guard.Actor.to_authorizable ~ctx subject in
  let* () = Guard.Persistence.validate db_ctx effects actor in
  Lwt.return_ok ()
;;

let recruiter_can_update_contact_language =
  Test_utils.case
  @@ fun () ->
  let open Utils.Lwt_result.Infix in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let ctx = Database.to_ctx db_ctx in
  let%lwt actor =
    let open Guard.Persistence in
    ActorRole.find_actors_by_role ~ctx (`Recruiter, None)
    ||> (function
     | [] -> failwith "No actors with role `Recruiter found."
     | hd :: _ -> hd)
    >|> Actor.find db_ctx
    ||> CCResult.get_or_failwith
  in
  let%lwt actual = update_language_as db_ctx actor in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a contact."
    (Ok ())
    actual;
  Lwt.return_ok ()
;;

let guest_cannot_update_language _ () =
  let%lwt actual =
    Test_utils.Data.connection
    @@ fun db_ctx -> update_language_as db_ctx Guard.guest_authorizable
  in
  Alcotest.(check (result unit Test_utils.error))
    "Guest cannot update a contact."
    (Error (Pool_message.Error.authorization "Failure"))
    (CCResult.map_err (fun _ -> Pool_message.Error.authorization "Failure") actual)
  |> Lwt.return
;;

let operator_works _ () =
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let ctx = Database.to_ctx db_ctx in
  let%lwt actual =
    let open Utils.Lwt_result.Infix in
    let open Guard in
    let to_error = Pool_message.Error.authorization in
    let target =
      "chris@gmail.com" |> Contact_test.contact_info |> Contact_test.create_contact true
    in
    let* _ = Contact.Guard.Actor.to_authorizable ~ctx target in
    let subject =
      "john@gmail.com" |> Contact_test.contact_info |> Contact_test.create_contact true
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
      |> Persistence.RolePermission.insert db_ctx
      >|- to_error
      ||> CCFun.tap (fun _ -> Persistence.Cache.clear ())
    in
    let effects = ValidationSet.one_of_tuple (Permission.Manage, `Contact, None) in
    let* () = Persistence.validate db_ctx effects actor in
    Lwt_result.return ()
  in
  Alcotest.(check (result unit Test_utils.error)) "Parametric roles work." (Ok ()) actual
  |> Lwt.return
;;

(* Regression: a failed [Actor.find] must not be cached, otherwise the actor stays
   unresolvable after its creation until the cache is cleared. The cache is
   intentionally not cleared between the calls. *)
let find_actor_after_failed_lookup _ () =
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx db_ctx in
  let contact =
    "lookup.before.create@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let actor_uuid = contact |> Contact.id |> Guard.Uuid.actor_of Contact.Id.value in
  let%lwt before = Guard.Persistence.Actor.find db_ctx actor_uuid in
  let () =
    Alcotest.(check bool)
      "Lookup of an unknown actor fails"
      true
      (CCResult.is_error before)
  in
  let%lwt created =
    Contact.Guard.Actor.to_authorizable ~ctx contact ||> CCResult.get_exn
  in
  let%lwt after = Guard.Persistence.Actor.find db_ctx actor_uuid in
  let uuid =
    CCResult.map (fun { Guard.Actor.uuid; _ } -> Guard.Uuid.Actor.to_string uuid)
  in
  Alcotest.(check (result string string))
    "Actor is found after its creation"
    (uuid (Ok created))
    (uuid after)
  |> Lwt.return
;;
