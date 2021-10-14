module Root_command = Cqrs_command.Root_command

let create_root () =
  let email = "test@econ.uzh.ch" in
  let password = "sihlsihl" in
  let firstname = "Woofy" in
  let lastname = "Woofer" in
  let command =
    CCResult.get_exn
    @@ Root_command.Create.decode
         [ "email", [ email ]
         ; "password", [ password ]
         ; "firstname", [ firstname ]
         ; "lastname", [ lastname ]
         ]
  in
  let events = Root_command.Create.handle command in
  let expected =
    let open Common_user in
    let ( let* ) = Result.bind in
    let* email = email |> Email.Address.create in
    let* password = password |> Password.create in
    let* firstname = firstname |> Firstname.create in
    let* lastname = lastname |> Lastname.create in
    let create = Root.{ email; password; firstname; lastname } in
    Ok [ Root.Created create |> Pool_event.root ]
  in
  Alcotest.(
    check (result (list Test_utils.event) string) "succeeds" expected events)
;;

let create_root_with_invalid_password () =
  let email = "test@econ.uzh.ch" in
  let password = "e" in
  let firstname = "Woofy" in
  let lastname = "Woofer" in
  let command =
    CCResult.get_exn
    @@ Root_command.Create.decode
         [ "email", [ email ]
         ; "password", [ password ]
         ; "firstname", [ firstname ]
         ; "lastname", [ lastname ]
         ]
  in
  let events = Root_command.Create.handle command in
  let expected = Error "password_policy_text" in
  Alcotest.(
    check (result (list Test_utils.event) string) "succeeds" expected events)
;;
