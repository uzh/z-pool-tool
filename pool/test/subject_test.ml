module Subject_command = Cqrs_command.Subject_command

let sign_up_not_allowed_suffix () =
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
    in
    let command =
      CCResult.get_exn
      @@ Subject_command.SignUp.decode
           Pool_common.Message.Field.
             [ Email |> show, [ "john@bluewin.com" ]
             ; Password |> show, [ "password" ]
             ; Firstname |> show, [ "Jane" ]
             ; Lastname |> show, [ "Doe" ]
             ; ( RecruitmentChannel |> show
               , [ Subject.RecruitmentChannel.Friend
                   |> Subject.RecruitmentChannel.show
                 ] )
             ]
    in
    Subject_command.SignUp.handle None command ~allowed_email_suffixes
  in
  let expected = Error Pool_common.Message.(Invalid Field.EmailSuffix) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let sign_up () =
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
    in
    let command =
      CCResult.get_exn
      @@ Subject_command.SignUp.decode
           Pool_common.Message.Field.
             [ Email |> show, [ "john@gmail.com" ]
             ; Password |> show, [ "password" ]
             ; Firstname |> show, [ "Jane" ]
             ; Lastname |> show, [ "Doe" ]
             ; ( RecruitmentChannel |> show
               , [ Subject.RecruitmentChannel.Friend
                   |> Subject.RecruitmentChannel.show
                 ] )
             ]
    in
    Subject_command.SignUp.handle
      (Some Pool_common.Language.En)
      command
      ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
