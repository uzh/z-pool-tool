include Entity.SmtpAuth

module Server = struct
  include Server

  let t = Caqti_type.string
end

module Port = struct
  include Port

  let t = Caqti_type.string
end

module Username = struct
  include Username

  let t = Caqti_type.string
end

module Password = struct
  include Password

  let t = Caqti_type.string
end

module AuthenticationMethod = struct
  include AuthenticationMethod

  let t = Caqti_type.string
end

module Protocol = struct
  include Protocol

  let t = Caqti_type.string
end

let t =
  let encode m =
    Ok
      ( m.Entity.SmtpAuth.server
      , (m.port, (m.username, (m.authentication_method, m.protocol))) )
  in
  let decode (server, (port, (username, (authentication_method, protocol)))) =
    let ( let* ) = Result.bind in
    let* server = Server.create server in
    let* port = Port.create port in
    let* username = Username.create username in
    let* authentication_method =
      AuthenticationMethod.create authentication_method
    in
    let* protocol = Protocol.create protocol in
    Ok { server; port; username; authentication_method; protocol }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Server.t
         (tup2
            Port.t
            (tup2 Username.t (tup2 AuthenticationMethod.t Protocol.t)))))
;;

module Write = struct
  open Entity.SmtpAuth.Write

  let t =
    let encode m =
      Ok
        ( m.Entity.SmtpAuth.Write.server
        , ( m.port
          , (m.username, (m.password, (m.authentication_method, m.protocol))) )
        )
    in
    let decode
        ( server
        , (port, (username, (password, (authentication_method, protocol)))) )
      =
      let ( let* ) = Result.bind in
      let* server = Server.create server in
      let* port = Port.create port in
      let* username = Username.create username in
      let* password = Password.create password in
      let* authentication_method =
        AuthenticationMethod.create authentication_method
      in
      let* protocol = Protocol.create protocol in
      Ok { server; port; username; password; authentication_method; protocol }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Server.t
           (tup2
              Port.t
              (tup2
                 Username.t
                 (tup2 Password.t (tup2 AuthenticationMethod.t Protocol.t))))))
  ;;
end
