open CCFun
include Entity.SmtpAuth
module Id = Pool_common.Repo.Id

module Label = struct
  include Label

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Server = struct
  include Server

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Port = struct
  include Port

  let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
end

module Username = struct
  include Username

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Password = struct
  include Password

  let t =
    let open Utils.Crypto.String in
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (decrypt_from_string
       %> CCResult.map_err (fun _ ->
         Pool_common.Message.(Decode Field.DatabaseUrl)))
      encrypt_to_string
  ;;
end

module Mechanism = Pool_common.Repo.Model.SelectorType (Mechanism)
module Protocol = Pool_common.Repo.Model.SelectorType (Protocol)

module Default = struct
  include Default

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      CCFun.(create %> CCResult.return)
      value
  ;;
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( m.label
        , ( m.server
          , (m.port, (m.username, (m.mechanism, (m.protocol, m.default)))) ) )
      )
  in
  let decode
    (id, (label, (server, (port, (username, (mechanism, (protocol, default)))))))
    =
    Ok { id; label; server; port; username; mechanism; protocol; default }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Id.t
         (t2
            Label.t
            (t2
               Server.t
               (t2
                  Port.t
                  (t2
                     (option Username.t)
                     (t2 Mechanism.t (t2 Protocol.t Default.t))))))))
;;

module Write = struct
  open Entity.SmtpAuth.Write

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.label
          , ( m.server
            , ( m.port
              , ( m.username
                , (m.password, (m.mechanism, (m.protocol, m.default))) ) ) ) )
        )
    in
    let decode
      ( id
      , ( label
        , ( server
          , (port, (username, (password, (mechanism, (protocol, default))))) )
        ) )
      =
      let open CCResult in
      Ok
        { id
        ; label
        ; server
        ; port
        ; username
        ; password
        ; mechanism
        ; protocol
        ; default
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              Label.t
              (t2
                 Server.t
                 (t2
                    Port.t
                    (t2
                       (option Username.t)
                       (t2
                          (option Password.t)
                          (t2 Mechanism.t (t2 Protocol.t Default.t)))))))))
  ;;
end
