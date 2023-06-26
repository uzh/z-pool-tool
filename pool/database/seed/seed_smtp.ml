let get_or_failwith = Pool_common.Utils.get_or_failwith

let create label =
  let open Email in
  let open Email.SmtpAuth in
  let server, port, username, password, mechanism, protocol, default =
    ( "smtp.uzh.ch"
    , 25
    , None
    , None
    , Mechanism.PLAIN
    , Protocol.STARTTLS
    , Default.create true )
  in
  Write.create
    (Label.create (label |> Pool_database.Label.value) |> get_or_failwith)
    (Server.create server |> get_or_failwith)
    (Port.create port |> get_or_failwith)
    (CCOption.map CCFun.(Username.create %> get_or_failwith) username)
    (CCOption.map CCFun.(Password.create %> get_or_failwith) password)
    mechanism
    protocol
    default
  |> get_or_failwith
  |> fun smtp -> handle_event label (SmtpCreated smtp)
;;
