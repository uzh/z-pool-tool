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
    let open CCResult in
    let open CCFun in
    Caqti_type.(
      custom
        ~encode:
          (Utils.Crypto.String.encrypt_to_string %> of_string %> CCResult.pure)
        ~decode:(fun m ->
          map_err (fun _ ->
            let open Pool_common in
            Utils.error_to_string
              Language.En
              Message.(Decode Field.SmtpPassword))
          @@ (m |> value |> Utils.Crypto.String.decrypt_from_string))
        string)
  ;;
end

module Mechanism = struct
  include Mechanism
  include Pool_common.Repo.Model.SelectorType (Core)
end

module Protocol = struct
  include Protocol
  include Pool_common.Repo.Model.SelectorType (Core)
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , (m.label, (m.server, (m.port, (m.username, (m.mechanism, m.protocol)))))
      )
  in
  let decode (id, (label, (server, (port, (username, (mechanism, protocol)))))) =
    Ok { id; label; server; port; username; mechanism; protocol }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Label.t
            (tup2
               Server.t
               (tup2
                  Port.t
                  (tup2 (option Username.t) (tup2 Mechanism.t Protocol.t)))))))
;;

module Write = struct
  open Entity.SmtpAuth.Write

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.label
          , ( m.server
            , (m.port, (m.username, (m.password, (m.mechanism, m.protocol)))) )
          ) )
    in
    let decode
      ( id
      , (label, (server, (port, (username, (password, (mechanism, protocol))))))
      )
      =
      let open CCResult in
      Ok { id; label; server; port; username; password; mechanism; protocol }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2
              Label.t
              (tup2
                 Server.t
                 (tup2
                    Port.t
                    (tup2
                       (option Username.t)
                       (tup2 (option Password.t) (tup2 Mechanism.t Protocol.t))))))))
  ;;
end
