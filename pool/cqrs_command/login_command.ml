module Conformist = Pool_conformist

let src = Logs.Src.create "login.cqrs"

module Create2FaLogin : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> email_job:(Pool_user.t -> Authentication.t -> Email.dispatch)
    -> Pool_user.t
    -> Authentication.t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  let handle ?(tags = Logs.Tag.empty) ~email_job user auth =
    let open CCResult in
    let open Authentication in
    Logs.info ~src (fun m -> m "Handle command Create2FALogin" ~tags);
    let email = email_job user auth in
    Ok [ Created auth |> Pool_event.authentication; Email.sent email |> Pool_event.email ]
  ;;
end

module Confirm2FaLogin : sig
  val decode
    :  Conformist.input
    -> (Authentication.Id.t * Authentication.Token.t, Pool_message.Error.t) result

  val handle
    :  ?tags:Logs.Tag.set
    -> Authentication.t
    -> Authentication.Token.t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  open Authentication

  let schema = Conformist.(make Field.[ Id.schema (); Token.schema () ] CCPair.make)

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let handle ?(tags = Logs.Tag.empty) auth token =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Confirm2FALogin" ~tags);
    let* () =
      if Token.equal token auth.token
      then Ok ()
      else Error Pool_message.(Error.Invalid Field.Token)
    in
    Ok [ Deleted auth |> Pool_event.authentication ]
  ;;
end
