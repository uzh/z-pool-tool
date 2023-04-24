module Root_command = Cqrs_command.Root_command

module Data = struct
  let email = "test@econ.uzh.ch"
  let password = "Sihlsihl9?"
  let firstname = "Woofy"
  let lastname = "Woofer"
end

let create_root () =
  let open Data in
  let command =
    CCResult.get_exn
    @@ Root_command.Create.decode
         Pool_common.Message.
           [ Field.(Email |> show), [ email ]
           ; Field.(Password |> show), [ password ]
           ; Field.(Firstname |> show), [ firstname ]
           ; Field.(Lastname |> show), [ lastname ]
           ]
  in
  let events = Root_command.Create.handle command in
  let expected =
    let open Pool_user in
    let open CCResult in
    let* email = email |> EmailAddress.create in
    let* password = password |> Password.create in
    let* firstname = firstname |> Firstname.create in
    let* lastname = lastname |> Lastname.create in
    let create =
      { id = None
      ; Admin.email
      ; password
      ; firstname
      ; lastname
      ; roles = Some (Guard.RoleSet.singleton `Root)
      }
    in
    Ok [ Admin.Created create |> Pool_event.admin ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let create_root_with_invalid_password () =
  let open Data in
  let password = "e" in
  let command =
    CCResult.get_exn
    @@ Root_command.Create.decode
         Pool_common.Message.
           [ Field.(Email |> show), [ email ]
           ; Field.(Password |> show), [ password ]
           ; Field.(Firstname |> show), [ firstname ]
           ; Field.(Lastname |> show), [ lastname ]
           ]
  in
  let events = Root_command.Create.handle command in
  let expected = Error Pool_common.(Message.PasswordPolicyMinLength 8) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
