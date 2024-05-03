module Command = Cqrs_command.Smtp_command

module Data = struct
  open Email.SmtpAuth

  let smtp_id = Id.create ()
  let smtp_label = Label.of_string "test-label-smtp"
  let server = Server.of_string "smtp.uzh.ch"
  let port = Port.create 25 |> Test_utils.get_or_failwith
  let username = Username.of_string "test@mail.com"
  let password = Password.of_string "password"
  let mechanism = Mechanism.LOGIN
  let protocol = Protocol.STARTTLS
  let default = Default.create true

  let write =
    Email.SmtpAuth.Write.
      { id = smtp_id
      ; label = smtp_label
      ; server
      ; port
      ; username = Some username
      ; password = Some password
      ; mechanism
      ; protocol
      ; default
      }
  ;;

  module Urlencoded = struct
    open Pool_message

    let label = Label.value smtp_label
    let server = Server.value server
    let port = Port.value port
    let username = Username.value username
    let password = Password.value password
    let mechanism = Mechanism.show mechanism
    let protocol = Protocol.show protocol
    let default = Default.value default

    let valid =
      [ Field.(show SmtpLabel), [ label ]
      ; Field.(show SmtpServer), [ server ]
      ; Field.(show SmtpPort), [ CCInt.to_string port ]
      ; Field.(show SmtpUsername), [ username ]
      ; Field.(show SmtpPassword), [ password ]
      ; Field.(show SmtpMechanism), [ mechanism ]
      ; Field.(show SmtpProtocol), [ protocol ]
      ; Field.(show DefaultSmtpServer), [ Utils.Bool.to_string default ]
      ]
    ;;

    let missing_username =
      [ Field.(show SmtpLabel), [ label ]
      ; Field.(show SmtpServer), [ server ]
      ; Field.(show SmtpPort), [ CCInt.to_string port ]
      ; Field.(show SmtpPassword), [ password ]
      ; Field.(show SmtpMechanism), [ mechanism ]
      ; Field.(show SmtpProtocol), [ protocol ]
      ; Field.(show DefaultSmtpServer), [ Utils.Bool.to_string default ]
      ]
    ;;
  end
end

let create_smtp_valid () =
  let open Data in
  let open CCResult.Infix in
  let event_id = System_event.Id.create () in
  let events =
    Command.Create.(
      Urlencoded.valid
      |> decode
      >>= smtp_of_command ~id:smtp_id
      >>= handle ~event_id None)
  in
  let expected =
    Ok
      [ Email.SmtpCreated write |> Pool_event.email
      ; System_event.(
          Job.SmtpAccountUpdated
          |> create ~id:event_id
          |> created
          |> Pool_event.system_event)
      ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let create_missing_username () =
  let open Data in
  let open CCResult.Infix in
  let event_id = System_event.Id.create () in
  let smtp_id = Email.SmtpAuth.Id.create () in
  let events =
    Command.Create.(
      Urlencoded.missing_username
      |> decode
      >>= smtp_of_command ~id:smtp_id
      >>= handle ~event_id None)
  in
  let expected = Error Pool_message.Error.SmtpLoginMissingCredentials in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
