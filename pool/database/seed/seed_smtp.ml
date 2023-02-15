let get_or_failwith = Pool_common.Utils.get_or_failwith

let create label =
  let open Pool_tenant in
  let open SmtpAuth in
  let server, port, username, password, mechanism, protocol =
    "smtp.uzh.ch", 25, None, None, Mechanism.PLAIN, Protocol.STARTTLS
  in
  Write.create
    (Label.create (label |> Pool_database.Label.value) |> get_or_failwith)
    (Server.create server |> get_or_failwith)
    (Port.create port |> get_or_failwith)
    (CCOption.map CCFun.(Username.create %> get_or_failwith) username)
    (CCOption.map CCFun.(Password.create %> get_or_failwith) password)
    mechanism
    protocol
  |> get_or_failwith
  |> fun smtp -> handle_event label (SmtpCreated smtp)
;;
