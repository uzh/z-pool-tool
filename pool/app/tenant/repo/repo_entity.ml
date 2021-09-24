open Entity
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module SmtpAuth = Repo_entity_smtp_auth
module Database = Repo_entity_database

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

module PartnerLogos = struct
  include PartnerLogos

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

let t =
  let encode m =
    Ok
      ( Id.value m.id
      , ( Title.value m.title
        , ( Description.value m.description
          , ( Url.value m.url
            , ( m.database
              , ( m.smtp_auth
                , ( Styles.value m.styles
                  , ( Icon.value m.icon
                    , ( Logos.value m.logos
                      , ( PartnerLogos.value m.partner_logos
                        , ( Maintenance.value m.maintenance
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
    let ( let* ) = Result.bind in
    let* title = Title.create title in
    let* description = Description.create description in
    let* url = Url.create url in
    let* styles = Styles.create styles in
    let* icon = Icon.create icon in
    let* logos = Logos.create logos in
    let* partner_logos = PartnerLogos.create partner_logos in
    Ok
      { id = Id.of_string id
      ; title
      ; description
      ; url
      ; database
      ; smtp_auth
      ; styles
      ; icon
      ; logos
      ; partner_logos
      ; maintenance = Maintenance.create maintenance
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
         RepoId.t
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
                                    PartnerLogos.t
                                    (tup2
                                       Maintenance.t
                                       (tup2
                                          Disabled.t
                                          (tup2
                                             Settings.Language.t
                                             (tup2 ptime ptime)))))))))))))))
;;

module Read = struct
  open Entity.Read

  let t =
    let encode m =
      Ok
        ( Id.value m.Read.id
        , ( Title.value m.title
          , ( Description.value m.description
            , ( Url.value m.url
              , ( m.smtp_auth
                , ( Styles.value m.styles
                  , ( Icon.value m.icon
                    , ( Logos.value m.logos
                      , ( PartnerLogos.value m.partner_logos
                        , ( Maintenance.value m.maintenance
                          , ( Disabled.value m.disabled
                            , (m.default_language, (m.created_at, m.updated_at))
                            ) ) ) ) ) ) ) ) ) ) )
    in
    let decode
        ( id
        , ( title
          , ( description
            , ( url
              , ( smtp_auth
                , ( styles
                  , ( icon
                    , ( logos
                      , ( partner_logos
                        , ( maintenance
                          , ( disabled
                            , (default_language, (created_at, updated_at)) ) )
                        ) ) ) ) ) ) ) ) )
      =
      let ( let* ) = Result.bind in
      let* title = Title.create title in
      let* description = Description.create description in
      let* url = Url.create url in
      let* styles = Styles.create styles in
      let* icon = Icon.create icon in
      let* logos = Logos.create logos in
      let* partner_logos = PartnerLogos.create partner_logos in
      Ok
        { id = Id.of_string id
        ; title
        ; description
        ; url
        ; smtp_auth
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; maintenance = Maintenance.create maintenance
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
           RepoId.t
           (tup2
              Title.t
              (tup2
                 Description.t
                 (tup2
                    Url.t
                    (tup2
                       SmtpAuth.t
                       (tup2
                          Styles.t
                          (tup2
                             Icon.t
                             (tup2
                                Logos.t
                                (tup2
                                   PartnerLogos.t
                                   (tup2
                                      Maintenance.t
                                      (tup2
                                         Disabled.t
                                         (tup2
                                            Settings.Language.t
                                            (tup2 ptime ptime))))))))))))))
  ;;
end
