open Entity

module Title = struct
  include Title

  let t = Caqti_type.string
end

module Description = struct
  include Description

  let t = Caqti_type.string
end

module Url = struct
  include Url

  let t = Caqti_type.string
end

module Database = struct
  include Database

  let t = Caqti_type.string
end

module Styles = struct
  include Styles

  let t = Caqti_type.string
end

module Icon = struct
  include Icon

  let t = Caqti_type.string
end

module Logos = struct
  include Logos

  let t = Caqti_type.string
end

module PartnerLogo = struct
  include PartnerLogo

  let t = Caqti_type.string
end

module Maintenance = struct
  include Maintenance

  let t = Caqti_type.bool
end

module Disabled = struct
  include Disabled

  let t = Caqti_type.bool
end

module SmtpAuth = struct
  include SmtpAuth

  module Server = struct
    include SmtpAuth.Server

    let t = Caqti_type.string
  end

  module Port = struct
    include SmtpAuth.Port

    let t = Caqti_type.string
  end

  module Username = struct
    include SmtpAuth.Username

    let t = Caqti_type.string
  end

  module AuthenticationMethod = struct
    include SmtpAuth.AuthenticationMethod

    let t = Caqti_type.string
  end

  module Protocol = struct
    include SmtpAuth.Protocol

    let t = Caqti_type.string
  end

  let t =
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
end

let t =
  let encode m =
    Ok
      ( Common.Id.show m.id
      , ( m.title
        , ( m.description
          , ( m.url
            , ( m.database
              , ( m.smtp_auth
                , ( m.styles
                  , ( m.icon
                    , ( m.logos
                      , ( m.partner_logos
                        , ( m.maintenance
                          , ( Disabled.value m.disabled
                            , (m.default_language, (m.created_at, m.updated_at))
                            ) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( title
        , ( description
          , ( url
            , ( database
              , ( smtp_auth
                , ( styles
                  , ( icon
                    , ( logos
                      , ( partner_logos
                        , ( maintenance
                          , ( disabled
                            , (default_language, (created_at, updated_at)) ) )
                        ) ) ) ) ) ) ) ) ) )
    =
    Ok
      { id = Common.Id.of_string id
      ; title
      ; description
      ; url
      ; database
      ; smtp_auth
      ; styles
      ; icon
      ; logos
      ; partner_logos
      ; maintenance
      ; disabled = Disabled.create disabled
      ; default_language
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common.Repo.Id.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  Url.t
                  (tup2
                     Database.t
                     (tup2
                        SmtpAuth.t
                        (tup2
                           Styles.t
                           (tup2
                              Icon.t
                              (tup2
                                 Logos.t
                                 (tup2
                                    PartnerLogo.t
                                    (tup2
                                       Maintenance.t
                                       (tup2
                                          Disabled.t
                                          (tup2
                                             Settings.Language.t
                                             (tup2 ptime ptime)))))))))))))))
;;

let find_by_id (id : string) : (t, string) result Lwt.t = Utils.todo id
let list_all = Utils.todo
let insert (t : t) = Utils.todo t
let update (t : t) = Utils.todo t
let destroy = Utils.todo
