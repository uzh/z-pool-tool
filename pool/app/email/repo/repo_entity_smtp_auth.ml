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
       %> CCResult.map_err (fun _ -> Pool_message.(Error.Decode Field.DatabaseUrl)))
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

module RateLimit = struct
  include RateLimit

  let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
end

module InvitationCapacity = struct
  include InvitationCapacity

  let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
end

module SystemAccount = struct
  include SystemAccount

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      CCFun.(create %> CCResult.return)
      value
  ;;
end

module InternalRegex = struct
  include InternalRegex

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( m.label
        , ( m.server
          , ( m.port
            , ( m.username
              , ( m.mechanism
                , ( m.protocol
                  , ( m.default
                    , ( m.system_account
                      , (m.internal_regex, (m.rate_limit, m.invitation_capacity)) ) ) ) )
              ) ) ) ) )
  in
  let decode
        ( id
        , ( label
          , ( server
            , ( port
              , ( username
                , ( mechanism
                  , ( protocol
                    , ( default
                      , ( system_account
                        , (internal_regex, (rate_limit, invitation_capacity)) ) ) ) ) ) )
            ) ) )
    =
    Ok
      { id
      ; label
      ; server
      ; port
      ; username
      ; mechanism
      ; protocol
      ; default
      ; system_account
      ; internal_regex
      ; rate_limit
      ; invitation_capacity
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
                        Mechanism.t
                        (t2
                           Protocol.t
                           (t2
                              Default.t
                              (t2
                                 SystemAccount.t
                                 (t2
                                    (option InternalRegex.t)
                                    (t2 RateLimit.t InvitationCapacity.t))))))))))))
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
                , ( m.password
                  , ( m.mechanism
                    , ( m.protocol
                      , ( m.default
                        , ( m.system_account
                          , (m.internal_regex, (m.rate_limit, m.invitation_capacity)) ) )
                      ) ) ) ) ) ) ) )
    in
    let decode
          ( id
          , ( label
            , ( server
              , ( port
                , ( username
                  , ( password
                    , ( mechanism
                      , ( protocol
                        , ( default
                          , ( system_account
                            , (internal_regex, (rate_limit, invitation_capacity)) ) ) ) )
                    ) ) ) ) ) )
      =
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
        ; system_account
        ; internal_regex
        ; rate_limit
        ; invitation_capacity
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
                          (t2
                             Mechanism.t
                             (t2
                                Protocol.t
                                (t2
                                   Default.t
                                   (t2
                                      SystemAccount.t
                                      (t2
                                         (option InternalRegex.t)
                                         (t2 RateLimit.t InvitationCapacity.t)))))))))))))
  ;;
end
