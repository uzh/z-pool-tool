include Entity.SmtpAuth

let create_result_to_option create_fcn =
  CCOption.map_or
    ~default:(Ok None)
    CCFun.(create_fcn %> CCResult.map CCOption.pure)
;;

module Server = struct
  include Server

  let t = Caqti_type.string
end

module Port = struct
  include Port

  let t = Caqti_type.int
end

module Username = struct
  include Username

  let t = Caqti_type.string
end

module Password = struct
  include Password

  let t =
    let open CCResult in
    let open CCFun in
    Caqti_type.(
      custom
        ~encode:(Utils.Crypto.String.encrypt_to_string %> CCResult.pure)
        ~decode:(fun m ->
          map_err (fun _ ->
            let open Pool_common in
            Utils.error_to_string
              Language.En
              Message.(Decode Field.SmtpPassword))
          @@ Utils.Crypto.String.decrypt_from_string m)
        string)
  ;;
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
    let open CCResult in
    map_err (fun _ ->
      let open Pool_common in
      Utils.error_to_string Language.En Message.(Decode Field.SmtpReadModel))
    @@ let* server = Server.create server in
       let* port = Port.create port in
       let* username = create_result_to_option Username.create username in
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
            (tup2 (option Username.t) (tup2 AuthenticationMethod.t Protocol.t)))))
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
      (server, (port, (username, (password, (authentication_method, protocol)))))
      =
      let open CCResult in
      map_err (fun _ ->
        let open Pool_common in
        Utils.error_to_string Language.En Message.(Decode Field.SmtpWriteModel))
      @@ let* server = Server.create server in
         let* port = Port.create port in
         let* username = create_result_to_option Username.create username in
         let* password = create_result_to_option Password.create password in
         let* authentication_method =
           AuthenticationMethod.create authentication_method
         in
         let* protocol = Protocol.create protocol in
         Ok
           { server; port; username; password; authentication_method; protocol }
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
                 (option Username.t)
                 (tup2
                    (option Password.t)
                    (tup2 AuthenticationMethod.t Protocol.t))))))
  ;;

  let schema_decoder create_fcn encode_fnc field name =
    let error_to_string = Pool_common.(Utils.error_to_string Language.En) in
    let decoder create_fcn field l =
      let open CCResult in
      match l with
      | x :: _ -> create_fcn x |> map_err error_to_string
      | [] -> Error (PoolError.Undefined field |> error_to_string)
    in
    Conformist.custom
      (decoder create_fcn field)
      (fun l -> l |> encode_fnc |> CCList.pure)
      name
  ;;

  let schema =
    let command server port username password authentication_method protocol =
      { server; port; username; password; authentication_method; protocol }
    in
    let open Entity_smtp_auth in
    let open PoolError.Field in
    Conformist.(
      make
        Field.
          [ Server.(schema_decoder create value SmtpAuthServer "SMTP_HOST")
          ; Port.(
              schema_decoder
                (fun str ->
                  let open CCResult in
                  CCInt.of_string str
                  |> CCOption.to_result PoolError.(NotANumber str)
                  >>= create)
                CCInt.to_string
                SmtpPort
                "SMTP_PORT")
            (* TODO wrap as pair as described in
               https://github.com/oxidizing/conformist/issues/11, once exists *)
          ; Conformist.optional
              Username.(
                schema_decoder create value SmtpUsername "SMTP_USERNAME")
          ; Conformist.optional
              Password.(
                schema_decoder create value SmtpPassword "SMTP_PASSWORD")
          ; AuthenticationMethod.(
              schema_decoder create value SmtpAuthMethod "SMTP_MECHANISM")
          ; Protocol.(schema_decoder create value SmtpProtocol "SMTP_PROTOCOL")
          ]
        command)
  ;;

  let load_from_environment () =
    try Ok (Sihl.Configuration.read schema) with
    | _ ->
      Error
        PoolError.(
          notfoundlist
            Field.Value
            [ "SMTP_HOST"
            ; "SMTP_PORT"
            ; "SMTP_USERNAME"
            ; "SMTP_PASSWORD"
            ; "SMTP_MECHANISM"
            ; "SMTP_PROTOCOL"
            ])
  ;;
end
