let update_language_as ~actor =
  let open Lwt_result.Syntax in
  let subject =
    "john@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let* tenant = Pool_tenant.find_by_label Test_utils.Data.database_label in
  let ctx = Pool_tenant.to_ctx Test_utils.Data.database_label in
  let effects = Cqrs_command.Contact_command.Update.effects tenant subject in
  let* (_ : [> `User ] Guard.Authorizable.t) =
    Guard.Pool_tenant.to_authorizable ~ctx tenant
  in
  let* (_ : [> `User ] Guard.Authorizable.t) =
    Guard.Contact.to_authorizable ~ctx subject
  in
  let* () =
    (Guard.Persistence.checker_of_effects ~ctx effects) ~actor
    |> Lwt_result.map_error Pool_common.Message.authorization
  in
  Lwt.return_ok ()
;;

let admin_can_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Guard.console_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a contact."
    (Ok ())
    actual
  |> Lwt.return
;;

let guest_cannot_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Guard.guest_authorizable in
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
    let* target' = Guard.Contact.to_authorizable ~ctx target in
    let subject =
      "john@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* actor = Guard.Contact.to_authorizable ~ctx subject in
    let* () =
      Guard.Persistence.grant_roles
        ~ctx
        actor.Guard.Authorizable.uuid
        (Guard.Role_set.singleton (`Operator target'.Guard.Authorizable.uuid))
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let* actor = Guard.Contact.to_authorizable ~ctx subject in
    let* () =
      Guard.Persistence.save_rule
        ~ctx
        ( `Entity (`Operator target'.Guard.Authorizable.uuid)
        , `Manage
        , `One target'.Guard.Authorizable.uuid )
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let effects = [ `Manage, `One target'.Guard.Authorizable.uuid ] in
    let* () =
      (Guard.Persistence.checker_of_effects ~ctx effects) ~actor
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
