open Entity
module Common = Pool_common
module SmtpAuth = Repo_entity_smtp_auth

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

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Caqti_type.string
  end
end

module Icon = struct
  include Icon

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Caqti_type.string
  end
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
  let open Entity.Read in
  let encode m =
    Ok
      ( Id.value m.Read.id
      , ( m.title
        , ( m.description
          , ( m.url
            , ( m.database_label
              , ( m.smtp_auth
                , ( m.styles
                  , ( m.icon
                    , ( m.partner_logos
                      , ( m.maintenance
                        , ( m.disabled
                          , (m.default_language, (m.created_at, m.updated_at))
                          ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( title
        , ( description
          , ( url
            , ( database_label
              , ( smtp_auth
                , ( styles
                  , ( icon
                    , ( partner_logos
                      , ( maintenance
                        , ( disabled
                          , (default_language, (created_at, updated_at)) ) ) )
                    ) ) ) ) ) ) ) )
    =
    let open CCResult in
    let* title = Title.create title in
    let* description = Description.create description in
    let* url = Url.create url in
    let* partner_logos = PartnerLogos.create partner_logos in
    Ok
      { id = Id.of_string id
      ; title
      ; description
      ; url
      ; database_label
      ; smtp_auth
      ; styles
      ; icon
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
         Common.Repo.Id.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  Url.t
                  (tup2
                     Common.Repo.Database.Label.t
                     (tup2
                        SmtpAuth.t
                        (tup2
                           Styles.t
                           (tup2
                              Icon.t
                              (tup2
                                 PartnerLogos.t
                                 (tup2
                                    Maintenance.t
                                    (tup2
                                       Disabled.t
                                       (tup2
                                          Settings.Language.t
                                          (tup2
                                             Common.Repo.CreatedAt.t
                                             Common.Repo.UpdatedAt.t))))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      Ok
        ( Id.value m.Write.id
        , ( m.title
          , ( m.description
            , ( m.url
              , ( m.database
                , ( m.smtp_auth
                  , ( m.styles
                    , ( m.icon
                      , ( m.partner_logos
                        , ( m.maintenance
                          , ( m.disabled
                            , (m.default_language, (m.created_at, m.updated_at))
                            ) ) ) ) ) ) ) ) ) ) )
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
                      , ( partner_logos
                        , ( maintenance
                          , ( disabled
                            , (default_language, (created_at, updated_at)) ) )
                        ) ) ) ) ) ) ) ) )
      =
      let open CCResult in
      let* title = Title.create title in
      let* description = Description.create description in
      let* url = Url.create url in
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
           Common.Repo.Id.t
           (tup2
              Title.t
              (tup2
                 Description.t
                 (tup2
                    Url.t
                    (tup2
                       Common.Repo.Database.t
                       (tup2
                          SmtpAuth.Write.t
                          (tup2
                             Styles.Write.t
                             (tup2
                                Icon.Write.t
                                (tup2
                                   PartnerLogos.t
                                   (tup2
                                      Maintenance.t
                                      (tup2
                                         Disabled.t
                                         (tup2
                                            Settings.Language.t
                                            (tup2
                                               Common.Repo.CreatedAt.t
                                               Common.Repo.UpdatedAt.t))))))))))))))
  ;;
end

module Selection = struct
  open Entity.Selection

  let t =
    let encode m = Ok (m.Selection.url, m.database_label) in
    let decode (url, database_label) =
      let open CCResult in
      let* url = Url.create url in
      Ok { url; database_label }
    in
    Caqti_type.(
      custom ~encode ~decode (tup2 Url.t Common.Repo.Database.Label.t))
  ;;
end
