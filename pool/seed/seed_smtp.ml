let get_or_failwith = Pool_common.Utils.get_or_failwith

let create db_ctx =
  let open Email in
  let open Email.SmtpAuth in
  let server, port, username, password, mechanism, protocol, default =
    ( "mailtrap-pool"
    , 1025
    , None
    , None
    , Mechanism.PLAIN
    , Protocol.STARTTLS
    , Default.create true )
  in
  Write.create
    (Label.create (db_ctx |> Database.label_of_ctx |> Database.Label.value) |> get_or_failwith)
    (Server.create server |> get_or_failwith)
    (Port.create port |> get_or_failwith)
    (CCOption.map CCFun.(Username.create %> get_or_failwith) username)
    (CCOption.map CCFun.(Password.create %> get_or_failwith) password)
    mechanism
    protocol
    default
  |> get_or_failwith
  |> fun smtp -> handle_event db_ctx (SmtpCreated smtp)
;;
