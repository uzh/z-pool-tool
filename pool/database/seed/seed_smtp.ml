let get_or_failwith = Pool_common.Utils.get_or_failwith

let create label =
  let open Pool_tenant in
  let server, port, username, password, authentication_method, protocol =
    "smtp.uzh.ch", 25, None, None, "PLAIN", "STARTTLS"
  in
  SmtpAuth.(
    Write.create
      (Label.create (label |> Pool_database.Label.value) |> get_or_failwith)
      (Server.create server |> get_or_failwith)
      (Port.create port |> get_or_failwith)
      (CCOption.map CCFun.(Username.create %> get_or_failwith) username)
      (CCOption.map CCFun.(Password.create %> get_or_failwith) password)
      (AuthenticationMethod.create authentication_method |> get_or_failwith)
      (Protocol.create protocol |> get_or_failwith)
    |> get_or_failwith)
  |> fun smtp -> Pool_tenant.handle_event label (SmtpCreated smtp)
;;
