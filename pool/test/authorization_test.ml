let update_language_as ~actor =
  let open Lwt_result.Syntax in
  let subject =
    "john@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let* tenant = Pool_tenant.find_by_label Test_utils.Data.database_label in
  let effects = Cqrs_command.Contact_command.Update.effects tenant subject in
  let* _ = Ocauth.Pool_tenant.to_authorizable tenant in
  let* _ = Ocauth.Contact.to_authorizable subject in
  let* () =
    (Ocauth.Persistence.checker_of_effects effects) ~actor
    |> Lwt_result.map_error Pool_common.Message.authorization
  in
  Lwt.return_ok ()
;;

let admin_can_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Ocauth.console_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a contact."
    (Ok ())
    actual
  |> Lwt.return
;;

let guest_cannot_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Ocauth.guest_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Guest cannot update a contact."
    (Error (Pool_common.Message.authorization "Failure"))
    (CCResult.map_err
       (fun _err -> Pool_common.Message.authorization "Failure")
       actual)
  |> Lwt.return
;;

let operator_works _switch () =
  let%lwt actual =
    let open Lwt_result.Syntax in
    let target =
      "chris@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* target' = Ocauth.Contact.to_authorizable target in
    let subject =
      "john@gmail.com"
      |> Contact_test.contact_info
      |> Contact_test.create_contact true
    in
    let* actor = Ocauth.Contact.to_authorizable subject in
    let* () =
      Ocauth.Persistence.grant_roles
        actor.Ocauth.Authorizable.uuid
        (Ocauth.Role_set.singleton (`Operator target'.Ocauth.Authorizable.uuid))
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let* actor = Ocauth.Contact.to_authorizable subject in
    let* () =
      Ocauth.Persistence.put_perm
        ( `Role (`Operator target'.Ocauth.Authorizable.uuid)
        , `Manage
        , `Uniq target'.Ocauth.Authorizable.uuid )
      |> Lwt_result.map_error Pool_common.Message.authorization
    in
    let effects = [ `Manage, `Uniq target'.Ocauth.Authorizable.uuid ] in
    let* () =
      (Ocauth.Persistence.checker_of_effects effects) ~actor
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
