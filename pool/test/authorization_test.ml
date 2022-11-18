let update_language_as actor =
  let open Guard in
  let open Lwt_result.Syntax in
  let subject =
    "john@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let* tenant = Pool_tenant.find_by_label Test_utils.Data.database_label in
  let ctx = Pool_tenant.to_ctx Test_utils.Data.database_label in
  let effects = Cqrs_command.Contact_command.Update.effects tenant subject in
  let* (_ : [> `Contact ] AuthorizableTarget.t) =
    PoolTenantTarget.to_authorizable ~ctx tenant
  in
  let* (_ : [> `Contact ] AuthorizableTarget.t) =
    ContactTarget.to_authorizable ~ctx subject
  in
  let* (_ : [> `Contact ] Authorizable.t) =
    PoolTenant.to_authorizable ~ctx tenant
  in
  let* (_ : [> `Contact ] Authorizable.t) =
    Contact.to_authorizable ~ctx subject
  in
  let* () =
    (Persistence.checker_of_effects ~ctx effects) actor
    |> Lwt_result.map_error Pool_common.Message.authorization
  in
  Lwt.return_ok ()
;;

let admin_can_update_language _switch () =
  let%lwt actual = update_language_as Guard.console_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a contact."
    (Ok ())
    actual
  |> Lwt.return
;;

let guest_cannot_update_language _switch () =
  let%lwt actual = update_language_as Guard.guest_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Guest cannot update a contact."
    (Error (Pool_common.Message.authorization "Failure"))
    (CCResult.map_err
       (fun _err -> Pool_common.Message.authorization "Failure")
       actual)
  |> Lwt.return
;;

let operator_works _ () =
  let ctx = Pool_tenant.to_ctx Test_utils.Data.database_label in
  let%lwt actual =
    let open Lwt_result.Syntax in
    let target =
      "chris@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* _ = Guard.Contact.to_authorizable ~ctx target in
    let* target' = Guard.ContactTarget.to_authorizable ~ctx target in
    let subject =
      "john@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* actor = Guard.Contact.to_authorizable ~ctx subject in
    let* () =
      Guard.Persistence.Actor.grant_roles
        ~ctx
        actor.Guard.Authorizable.uuid
        (Guard.ActorRoleSet.singleton
           (`Operator target'.Guard.AuthorizableTarget.uuid))
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let* actor = Guard.Contact.to_authorizable ~ctx subject in
    let* () =
      Guard.Persistence.Actor.save_rule
        ~ctx
        ( `ActorEntity (`Operator target'.Guard.AuthorizableTarget.uuid)
        , `Manage
        , `Target target'.Guard.AuthorizableTarget.uuid )
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let effects = [ `Manage, `Target target'.Guard.AuthorizableTarget.uuid ] in
    let* () =
      Guard.Persistence.checker_of_effects ~ctx effects actor
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    Lwt_result.return ()
  in
  Alcotest.(check (result unit Test_utils.error))
    "Parametric roles work."
    (Ok ())
    actual
  |> Lwt.return
;;
