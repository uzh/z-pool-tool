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
  let system_account = SystemAccount.create false
  let rate_limit = Email.SmtpAuth.RateLimit.default
  let invitation_capacity = Email.SmtpAuth.InvitationCapacity.default

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
      ; system_account
      ; internal_regex = None
      ; rate_limit
      ; invitation_capacity
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
    let system_account = SystemAccount.value system_account
    let rate_limit = Email.SmtpAuth.RateLimit.value rate_limit
    let invitation_capacity = Email.SmtpAuth.InvitationCapacity.value invitation_capacity

    let valid =
      [ Field.(show SmtpLabel), [ label ]
      ; Field.(show SmtpServer), [ server ]
      ; Field.(show SmtpPort), [ CCInt.to_string port ]
      ; Field.(show SmtpUsername), [ username ]
      ; Field.(show SmtpPassword), [ password ]
      ; Field.(show SmtpMechanism), [ mechanism ]
      ; Field.(show SmtpProtocol), [ protocol ]
      ; Field.(show DefaultSmtpServer), [ Utils.Bool.to_string default ]
      ; Field.(show SmtpSystemAccount), [ Utils.Bool.to_string system_account ]
      ; Field.(show SmtpRateLimit), [ CCInt.to_string rate_limit ]
      ; Field.(show SmtpInvitationCapacity), [ CCInt.to_string invitation_capacity ]
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
      ; Field.(show SmtpSystemAccount), [ Utils.Bool.to_string system_account ]
      ; Field.(show SmtpRateLimit), [ CCInt.to_string rate_limit ]
      ; Field.(show SmtpInvitationCapacity), [ CCInt.to_string invitation_capacity ]
      ]
    ;;
  end
end

let create_smtp_valid () =
  let open Data in
  let open CCResult.Infix in
  let open Email.SmtpAuth in
  let events =
    let open Command.Create in
    let event_id = System_event.Id.create () in
    Urlencoded.valid |> decode >>= smtp_of_command ~id:smtp_id >>= handle ~event_id None
  in
  let[@warning "-4"] smtp_created =
    match events with
    | Ok Pool_event.[ Email (Email.SmtpCreated smtp); SystemEvent _ ] -> smtp
    | Ok _ -> failwith "Unexpected event shape"
    | Error err ->
      let msg = Pool_common.(Utils.error_to_string Language.En err) in
      failwith msg
  in
  let () =
    Alcotest.(check bool "smtp id" true (Id.equal smtp_id smtp_created.Write.id))
  in
  let () =
    Alcotest.(
      check
        string
        "smtp label"
        (Label.value write.Write.label)
        (Label.value smtp_created.Write.label))
  in
  let () =
    Alcotest.(
      check
        int
        "rate limit"
        (RateLimit.value write.Write.rate_limit)
        (RateLimit.value smtp_created.Write.rate_limit))
  in
  Alcotest.(
    check
      int
      "invitation capacity"
      (InvitationCapacity.value write.Write.invitation_capacity)
      (InvitationCapacity.value smtp_created.Write.invitation_capacity))
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
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;
