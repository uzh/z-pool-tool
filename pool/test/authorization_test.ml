let ( >|= ) = Lwt.Infix.( >|= )
let database_label = Test_utils.Data.database_label

let update_language_as ~actor =
  let open Lwt_result.Syntax in
  let subject =
    "john@gmail.com"
    |> Contact_test.contact_info
    |> Contact_test.create_contact true
  in
  let language = Pool_common.Language.De in
  let* tenant = Pool_tenant.find_by_label database_label in
  let* effects =
    Cqrs_command.Contact_command.Update.get_effects tenant subject
  in
  let* _ = Ocauth.Contact.to_authorizable subject in
  let* () =
    let* rules = Ocauth.Persistence.collect_rules effects in
    (Ocauth.checker_of_rules rules) ~actor |> Lwt_result.lift
  in
  let* _events =
    Cqrs_command.Contact_command.Update.(
      [ ( Pool_common.Message.Field.(Language |> show)
        , [ language |> Pool_common.Language.show ] )
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject
      |> Lwt_result.lift)
  in
  Lwt.return_ok ()
;;

let admin_can_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Ocauth.console_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a subject."
    (Ok ())
    actual
  |> Lwt.return
;;

let guest_cannot_update_language _switch () =
  let%lwt actual = update_language_as ~actor:Ocauth.guest_authorizable in
  Alcotest.(check (result unit Test_utils.error))
    "Admin can update a subject."
    (Error (Pool_common.Message.authorization "Failure"))
    (CCResult.map_err
       (fun _err -> Pool_common.Message.authorization "Failure")
       actual)
  |> Lwt.return
;;